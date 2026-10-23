(* ascii.ml — a character-cell rendering surface.

   A surface is a grid of glyph ids, a colour band and a depth per cell.
   Simulations and scenes write cells with [put] / [put_z]; [flush] then draws
   the grid one row-string per colour band, so a full-screen frame costs a few
   hundred fillText calls instead of one per cell.

   Colours come from CSS custom properties ([<prefix>-0] .. [<prefix>-3]) so
   every surface follows the active theme without knowing about it. *)

open Brr
open Brr_canvas

let font_family = "\"IBM Plex Mono\", ui-monospace, Menlo, Consolas, monospace"
let bands = 4

(* ---- Glyphs ----------------------------------------------------------- *)

(* Glyph ids index a growable table of one-cell strings; 0 is blank. Keep to
   characters the mono font actually has, or row alignment drifts. *)
let glyph_tbl = ref [| " " |]
let glyph_ids : (string, int) Hashtbl.t = Hashtbl.create 64
let () = Hashtbl.replace glyph_ids " " 0

let glyph s =
  match Hashtbl.find_opt glyph_ids s with
  | Some i -> i
  | None ->
    let i = Array.length !glyph_tbl in
    glyph_tbl := Array.append !glyph_tbl [| s |];
    Hashtbl.replace glyph_ids s i;
    i

(* [ramp "abc"] is the glyph ids of each character of a UTF-8 string. *)
let ramp s =
  let rec go i acc =
    if i >= String.length s then Array.of_list (List.rev acc)
    else
      let n = Uchar.utf_decode_length (String.get_utf_8_uchar s i) in
      go (i + n) (glyph (String.sub s i n) :: acc)
  in
  go 0 []

(* Pick from a ramp with [x] in [0, 1]. *)
let pick r x =
  let n = Array.length r in
  let i = int_of_float (x *. float_of_int n) in
  r.(if i < 0 then 0 else if i >= n then n - 1 else i)

(* ---- Theme palette ---------------------------------------------------- *)

module Palette = struct
  let theme () =
    match El.at (Jstr.v "data-theme") (Document.root G.document) with
    | Some t -> Jstr.to_string t
    | None -> ""

  let cache : (string, string * C2d.style array) Hashtbl.t = Hashtbl.create 4

  let read prefix =
    let root = El.to_jv (Document.root G.document) in
    let cs = Jv.call Jv.global "getComputedStyle" [| root |] in
    Array.init bands (fun i ->
      let name = Printf.sprintf "%s-%d" prefix i in
      let v = Jv.to_string (Jv.call cs "getPropertyValue" [| Jv.of_string name |]) in
      C2d.color (Jstr.v (match String.trim v with "" -> "#888" | v -> v)))

  (* Re-read only when the theme attribute changes. *)
  let get prefix =
    let th = theme () in
    match Hashtbl.find_opt cache prefix with
    | Some (t, a) when t = th -> a
    | _ ->
      let a = read prefix in
      Hashtbl.replace cache prefix (th, a);
      a
end

(* ---- Surface ---------------------------------------------------------- *)

type fit = Window | Element

type t = {
  canvas : Canvas.t;
  ctx : C2d.t;
  fit : fit;
  palette : string;
  mutable font_px : float;
  line : float;                     (* line height, in font sizes *)
  mutable w : float;                (* css px *)
  mutable h : float;
  mutable cw : float;               (* cell size, css px *)
  mutable ch : float;
  mutable cols : int;
  mutable rows : int;
  mutable glyphs : int array;
  mutable band : int array;
  mutable depth : float array;
  buf : Buffer.t;
}

let el s = Canvas.to_el s.canvas
let font s = Jstr.v (Printf.sprintf "%gpx %s" s.font_px font_family)

let css_size s =
  match s.fit with
  | Window ->
    Jv.to_float (Jv.get Jv.global "innerWidth"), Jv.to_float (Jv.get Jv.global "innerHeight")
  | Element ->
    let e = El.to_jv (el s) in
    Jv.to_float (Jv.get e "clientWidth"), Jv.to_float (Jv.get e "clientHeight")

(* Re-fit the backing store and cell grid. Returns [true] when the grid
   changed, so owners can reallocate their own per-cell state. *)
let fit ?(force = false) s =
  let w, h = css_size s in
  if not force && w = s.w && h = s.h then false
  else begin
    let dpr = Window.device_pixel_ratio G.window in
    Canvas.set_w s.canvas (int_of_float (Float.round (w *. dpr)));
    Canvas.set_h s.canvas (int_of_float (Float.round (h *. dpr)));
    C2d.set_transform' s.ctx ~a:dpr ~b:0. ~c:0. ~d:dpr ~e:0. ~f:0.;
    C2d.set_font s.ctx (font s);
    s.w <- w;
    s.h <- h;
    s.cw <- Float.max 1. (C2d.Text_metrics.width (C2d.measure_text s.ctx (Jstr.v "M")));
    s.ch <- s.font_px *. s.line;
    let cols = int_of_float (Float.ceil (w /. s.cw)) and rows = int_of_float (Float.ceil (h /. s.ch)) in
    let resized = cols <> s.cols || rows <> s.rows in
    s.cols <- cols;
    s.rows <- rows;
    if resized then begin
      s.glyphs <- Array.make (cols * rows) 0;
      s.band <- Array.make (cols * rows) 0;
      s.depth <- Array.make (cols * rows) neg_infinity
    end;
    resized
  end

let create ?(at = []) ?(line = 1.2) ~fit:f ~palette ~font_px () =
  let canvas = Canvas.create [] in
  let e = Canvas.to_el canvas in
  List.iter (fun (k, v) -> El.set_at (Jstr.v k) (Some (Jstr.v v)) e) at;
  El.set_at (Jstr.v "aria-hidden") (Some (Jstr.v "true")) e;
  { canvas; ctx = C2d.get_context canvas; fit = f; palette; font_px; line;
    w = -1.; h = -1.; cw = 1.; ch = 1.; cols = 0; rows = 0;
    glyphs = [||]; band = [||]; depth = [||]; buf = Buffer.create 512 }

let clear s =
  Array.fill s.glyphs 0 (Array.length s.glyphs) 0;
  Array.fill s.depth 0 (Array.length s.depth) neg_infinity

let put s c r g b =
  if c >= 0 && r >= 0 && c < s.cols && r < s.rows then begin
    let i = r * s.cols + c in
    Array.unsafe_set s.glyphs i g;
    Array.unsafe_set s.band i b
  end

(* Depth-tested write; larger [z] is nearer the viewer. *)
let put_z s c r z g b =
  if c >= 0 && r >= 0 && c < s.cols && r < s.rows then begin
    let i = r * s.cols + c in
    if z > Array.unsafe_get s.depth i then begin
      Array.unsafe_set s.depth i z;
      Array.unsafe_set s.glyphs i g;
      Array.unsafe_set s.band i b
    end
  end

let flush s =
  let ctx = s.ctx and cols = s.cols in
  C2d.clear_rect ctx ~x:0. ~y:0. ~w:s.w ~h:s.h;
  C2d.set_font ctx (font s);
  C2d.set_text_baseline ctx C2d.Text_baseline.top;
  let pal = Palette.get s.palette and tbl = !glyph_tbl in
  let y_pad = (s.ch -. s.font_px) /. 2. in
  for b = 0 to bands - 1 do
    C2d.set_fill_style ctx pal.(b);
    for r = 0 to s.rows - 1 do
      let row = r * cols in
      Buffer.clear s.buf;
      let first = ref (-1) and pending = ref 0 in
      for c = 0 to cols - 1 do
        let g = Array.unsafe_get s.glyphs (row + c) in
        if g <> 0 && Array.unsafe_get s.band (row + c) = b then begin
          if !first < 0 then first := c
          else for _ = 1 to !pending do Buffer.add_char s.buf ' ' done;
          pending := 0;
          Buffer.add_string s.buf (Array.unsafe_get tbl g)
        end
        else if !first >= 0 then incr pending
      done;
      if !first >= 0 then
        C2d.fill_text ctx (Jstr.v (Buffer.contents s.buf))
          ~x:(float_of_int !first *. s.cw) ~y:(float_of_int r *. s.ch +. y_pad)
    done
  done

(* ---- Frame loop ------------------------------------------------------- *)

let prefers_reduced_motion () =
  let mq = Jv.call Jv.global "matchMedia" [| Jv.of_string "(prefers-reduced-motion: reduce)" |] in
  Jv.to_bool (Jv.get mq "matches")

(* One requestAnimationFrame loop for every animated thing on the page.
   Subscribers get the timestamp and a clamped frame delta in ms. With
   reduced motion the loop drops to a slow, steady tick. *)
module Ticker = struct
  let subs : (float -> float -> unit) list ref = ref []
  let running = ref false
  let last = ref 0.
  let min_gap = lazy (if prefers_reduced_motion () then 180. else 0.)

  let rec frame now =
    if now -. !last >= Lazy.force min_gap then begin
      let dt = if !last = 0. then 16.7 else Float.min 50. (now -. !last) in
      last := now;
      List.iter (fun f -> f now dt) !subs
    end;
    ignore (G.request_animation_frame frame)

  let add f =
    subs := !subs @ [ f ];
    if not !running then begin
      running := true;
      ignore (G.request_animation_frame frame)
    end
end

(* ---- Small browser helpers ------------------------------------------- *)

(* Calls [f true/false] as [e] enters and leaves the viewport. *)
let on_visibility e f =
  let io = Jv.get Jv.global "IntersectionObserver" in
  if Jv.is_none io then f true
  else
    let cb =
      Jv.callback ~arity:1 (fun entries ->
        for i = 0 to Jv.Jarray.length entries - 1 do
          f (Jv.to_bool (Jv.get (Jv.Jarray.get entries i) "isIntersecting"))
        done)
    in
    let obs = Jv.new' io [| cb; Jv.obj [| ("rootMargin", Jv.of_string "80px") |] |] in
    ignore (Jv.call obs "observe" [| El.to_jv e |])

(* Run [f] once the mono font is usable, so cell metrics are measured
   against the real face rather than a fallback. *)
let when_font_ready f =
  let started = ref false in
  let go () = if not !started then (started := true; f ()) in
  let fonts = Jv.get (Jv.get Jv.global "document") "fonts" in
  if Jv.is_none fonts then go ()
  else begin
    let p = Jv.call fonts "load" [| Jv.of_string ("13px " ^ font_family) |] in
    let k = Jv.callback ~arity:1 (fun _ -> go ()) in
    ignore (Jv.call p "then" [| k; k |]);
    ignore (G.set_timeout ~ms:1500 go)
  end
