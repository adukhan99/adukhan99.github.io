open Js_of_ocaml
open Js

module Style = Style
module Ui = Ui
module Ascii = Ascii
module Field = Field
module Scene = Scene
module Banner = Banner

let doc = Dom_html.document
let get_el sel = Opt.to_option (doc##querySelector (string sel))
let all sel f =
  let ns = doc##querySelectorAll (string sel) in
  for i = 0 to ns##.length - 1 do Opt.iter (ns##item i) f done

let every ms f = ignore (Dom_html.window##setInterval (wrap_callback f) (float_of_int ms))

(* Theme: phosphor (dark) by default, paper (light) on request. *)
let setup_theme () =
  let root = doc##.documentElement in
  let apply t = root##setAttribute (string "data-theme") (string t) in
  let stored =
    Optdef.case Dom_html.window##.localStorage (fun () -> None) (fun st ->
      Opt.to_option (st##getItem (string "theme")) |> Option.map to_string)
  in
  let theme = match stored with Some ("light" | "dark" as t) -> t | _ -> "dark" in
  apply theme;
  match get_el "#switch" with
  | None -> ()
  | Some s ->
    let s_in = Opt.get (Dom_html.CoerceTo.input s) (fun () -> failwith "not input") in
    s_in##.checked := bool (theme = "dark");
    s_in##.onchange := Dom_html.handler (fun _ ->
      let t = if to_bool s_in##.checked then "dark" else "light" in
      apply t;
      Optdef.iter Dom_html.window##.localStorage (fun st -> st##setItem (string "theme") (string t));
      _true)

let setup_email () =
  let decoded = to_string (Dom_html.window##atob (string "YWdkdWtoYW5AZ21haWwuY29t")) in
  all "[data-email-link='true']" (fun l -> l##setAttribute (string "href") (string ("mailto:" ^ decoded)))

let setup_navbar () =
  match get_el ".navbar" with
  | None -> ()
  | Some n ->
    let set_state () =
      let scrolled = (Unsafe.coerce Dom_html.window)##.scrollY in
      if to_float scrolled > 8.0 then n##.classList##add (string "scrolled") else n##.classList##remove (string "scrolled")
    in
    set_state ();
    let opts = Unsafe.obj [| ("passive", Unsafe.inject _true) |] in
    ignore (Unsafe.meth_call Dom_html.window "addEventListener"
      [| Unsafe.inject (string "scroll"); Unsafe.inject (wrap_callback (fun _ -> set_state ())); Unsafe.inject opts |])

(* York wall clock in the menubar. *)
let setup_clock () =
  match get_el "#clock" with
  | None -> ()
  | Some c ->
    let opts = Unsafe.obj [| ("timeZone", Unsafe.inject (string "Europe/London")); ("hour12", Unsafe.inject _false) |] in
    let tick () =
      let d = new%js date_now in
      c##.textContent := some (Unsafe.meth_call d "toLocaleTimeString" [| Unsafe.inject (string "en-GB"); Unsafe.inject opts |])
    in
    tick ();
    every 1000 tick

(* Type out [data-type] headings once, with a cursor. *)
let setup_typing () =
  let reduced = Ascii.prefers_reduced_motion () in
  all "[data-type]" (fun e ->
    let full = Opt.case (e##getAttribute (string "data-type")) (fun () -> "") to_string in
    e##setAttribute (string "aria-label") (string full);
    e##.classList##add (string "typing");
    if not reduced then begin
      let n = ref 0 in
      e##.textContent := some (string "");
      let rec step () =
        if !n <= String.length full then begin
          e##.textContent := some (string (String.sub full 0 !n));
          incr n;
          ignore (Dom_html.window##setTimeout (wrap_callback step) (float_of_int (28 + Random.int 50)))
        end
      in
      ignore (Dom_html.window##setTimeout (wrap_callback step) 600.)
    end)

(* Live numbers from the background simulation. *)
let setup_stats () =
  let set k v = all (Printf.sprintf "[data-stat='%s']" k) (fun e -> e##.textContent := some (string v)) in
  every 500 (fun () ->
    let s = Field.stats in
    if s.cols > 0 then begin
      set "grid" (Printf.sprintf "%d\xc3\x97%d cells" s.cols s.rows);
      set "fps" (Printf.sprintf "%.0f" s.fps);
      set "cover" (Printf.sprintf "%.1f%%" (100. *. s.cover));
      set "taps" (string_of_int s.taps)
    end)

let setup_logic () =
  Random.self_init ();
  setup_theme ();
  setup_email ();
  setup_navbar ();
  setup_clock ();
  setup_typing ();
  setup_stats ()
