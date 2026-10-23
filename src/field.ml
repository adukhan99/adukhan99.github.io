(* field.ml — the living background.

   Two coupled models on the character grid:
   - a Gray–Scott reaction–diffusion system tuned to the "mitosis" regime,
     so chemical spots grow and divide like cells;
   - an incompressible stable-fluids flow (Stam 1999) that advects them.

   Moving the pointer drags the fluid; a tap or click fires a vortex dipole,
   seeds new cells, drops a pulse of data-carrying "ink" and a shock ring.
   Scrolling couples in as a uniform drift. The grid is periodic, and its
   cells are not square, so every spatial operator weights x and y by the
   real cell size. *)

open Brr

(* Gray–Scott parameters (Pearson's classification, dt = 1). *)
let feed = 0.0367
let kill = 0.0649
let diff_a = 0.21
let diff_b = 0.105

type stats = {
  mutable cols : int;
  mutable rows : int;
  mutable fps : float;
  mutable cover : float;            (* fraction of cells above threshold *)
  mutable taps : int;
}

let stats = { cols = 0; rows = 0; fps = 60.; cover = 0.; taps = 0 }

type ring = { rx : float; ry : float; born : float }

type st = {
  s : Ascii.t;
  mutable n : int;
  mutable vx : float array; mutable vy : float array;
  mutable tx : float array; mutable ty : float array;   (* scratch *)
  mutable p : float array;
  mutable dv : float array;
  mutable a : float array; mutable b : float array;
  mutable a2 : float array; mutable b2 : float array;
  mutable ink : float array; mutable ink2 : float array;
  mutable boot : int;               (* frames of accelerated growth left *)
  mutable frame : int;
  mutable drift : float;            (* px/frame, from scrolling *)
  mutable rings : ring list;
}

(* Pointer state, in css px. *)
let px = ref (-1000.) and py = ref (-1000.)
let pdx = ref 0. and pdy = ref 0.
let pointer_in = ref false
let taps : (float * float) list ref = ref []

(* ---- Grid helpers ---------------------------------------------------- *)

let alloc st =
  let n = st.s.cols * st.s.rows in
  let z () = Array.make n 0. in
  st.n <- n;
  st.vx <- z (); st.vy <- z (); st.tx <- z (); st.ty <- z ();
  st.p <- z (); st.dv <- z ();
  st.a <- Array.make n 1.; st.b <- z (); st.a2 <- z (); st.b2 <- z ();
  st.ink <- z (); st.ink2 <- z ();
  stats.cols <- st.s.cols;
  stats.rows <- st.s.rows

(* Seed a small patch of chemical B centred on cell (c, r). *)
let seed st c r =
  let cols = st.s.cols and rows = st.s.rows in
  for dr = -1 to 1 do
    for dc = -2 to 2 do
      let cc = (c + dc + cols) mod cols and rr = (r + dr + rows) mod rows in
      let i = rr * cols + cc in
      st.a.(i) <- 0.5;
      st.b.(i) <- 0.25 +. Random.float 0.05
    done
  done

let scatter st count =
  for _ = 1 to count do
    seed st (Random.int (max 1 st.s.cols)) (Random.int (max 1 st.s.rows))
  done

(* Bilinear sample of a periodic field at fractional cell coordinates. *)
let sample (f : float array) cols rows x y =
  let fc = float_of_int cols and fr = float_of_int rows in
  let x = x -. (Float.floor (x /. fc) *. fc) and y = y -. (Float.floor (y /. fr) *. fr) in
  (* Wrapping a tiny negative can round up to exactly [fc]. *)
  let x = if x >= fc then 0. else x and y = if y >= fr then 0. else y in
  let i0 = int_of_float x and j0 = int_of_float y in
  let i1 = if i0 + 1 = cols then 0 else i0 + 1 and j1 = if j0 + 1 = rows then 0 else j0 + 1 in
  let sx = x -. float_of_int i0 and sy = y -. float_of_int j0 in
  let r0 = j0 * cols and r1 = j1 * cols in
  let f00 = Array.unsafe_get f (r0 + i0) and f10 = Array.unsafe_get f (r0 + i1) in
  let f01 = Array.unsafe_get f (r1 + i0) and f11 = Array.unsafe_get f (r1 + i1) in
  ((f00 *. (1. -. sx)) +. (f10 *. sx)) *. (1. -. sy) +. ((f01 *. (1. -. sx)) +. (f11 *. sx)) *. sy

(* Semi-Lagrangian advection of [src] into [dst] along the flow. *)
let advect st k (src : float array) (dst : float array) =
  let cols = st.s.cols and rows = st.s.rows in
  let kx = k /. st.s.cw and ky = k /. st.s.ch in
  let dy = st.drift *. ky in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      let i = (r * cols) + c in
      let x = float_of_int c -. (Array.unsafe_get st.vx i *. kx)
      and y = float_of_int r -. (Array.unsafe_get st.vy i *. ky) -. dy in
      Array.unsafe_set dst i (sample src cols rows x y)
    done
  done

(* Make the velocity field divergence-free (Gauss–Seidel pressure solve). *)
let project st iters =
  let cols = st.s.cols and rows = st.s.rows in
  let cw = st.s.cw and ch = st.s.ch in
  let ax = 1. /. (cw *. cw) and ay = 1. /. (ch *. ch) in
  let inv = 1. /. (2. *. (ax +. ay)) in
  let vx = st.vx and vy = st.vy and p = st.p and dv = st.dv in
  for r = 0 to rows - 1 do
    let up = (if r = 0 then rows - 1 else r - 1) * cols and dn = (if r = rows - 1 then 0 else r + 1) * cols in
    let row = r * cols in
    for c = 0 to cols - 1 do
      let l = if c = 0 then cols - 1 else c - 1 and rt = if c = cols - 1 then 0 else c + 1 in
      Array.unsafe_set dv (row + c)
        (((Array.unsafe_get vx (row + rt) -. Array.unsafe_get vx (row + l)) /. (2. *. cw))
         +. ((Array.unsafe_get vy (dn + c) -. Array.unsafe_get vy (up + c)) /. (2. *. ch)))
    done
  done;
  for _ = 1 to iters do
    for r = 0 to rows - 1 do
      let up = (if r = 0 then rows - 1 else r - 1) * cols and dn = (if r = rows - 1 then 0 else r + 1) * cols in
      let row = r * cols in
      for c = 0 to cols - 1 do
        let l = if c = 0 then cols - 1 else c - 1 and rt = if c = cols - 1 then 0 else c + 1 in
        Array.unsafe_set p (row + c)
          (((ax *. (Array.unsafe_get p (row + l) +. Array.unsafe_get p (row + rt)))
            +. (ay *. (Array.unsafe_get p (up + c) +. Array.unsafe_get p (dn + c)))
            -. Array.unsafe_get dv (row + c)) *. inv)
      done
    done
  done;
  for r = 0 to rows - 1 do
    let up = (if r = 0 then rows - 1 else r - 1) * cols and dn = (if r = rows - 1 then 0 else r + 1) * cols in
    let row = r * cols in
    for c = 0 to cols - 1 do
      let l = if c = 0 then cols - 1 else c - 1 and rt = if c = cols - 1 then 0 else c + 1 in
      let i = row + c in
      Array.unsafe_set vx i
        (Array.unsafe_get vx i -. ((Array.unsafe_get p (row + rt) -. Array.unsafe_get p (row + l)) /. (2. *. cw)));
      Array.unsafe_set vy i
        (Array.unsafe_get vy i -. ((Array.unsafe_get p (dn + c) -. Array.unsafe_get p (up + c)) /. (2. *. ch)))
    done
  done

(* One explicit Gray–Scott step. Laplacian weights are scaled so patterns
   come out round on screen despite tall cells. *)
let react st =
  let cols = st.s.cols and rows = st.s.rows in
  let ry = (st.s.cw /. st.s.ch) ** 2. in
  let a = st.a and b = st.b and a2 = st.a2 and b2 = st.b2 in
  for r = 0 to rows - 1 do
    let up = (if r = 0 then rows - 1 else r - 1) * cols and dn = (if r = rows - 1 then 0 else r + 1) * cols in
    let row = r * cols in
    for c = 0 to cols - 1 do
      let l = if c = 0 then cols - 1 else c - 1 and rt = if c = cols - 1 then 0 else c + 1 in
      let i = row + c in
      let ai = Array.unsafe_get a i and bi = Array.unsafe_get b i in
      let la =
        (Array.unsafe_get a (row + l) +. Array.unsafe_get a (row + rt) -. (2. *. ai))
        +. (ry *. (Array.unsafe_get a (up + c) +. Array.unsafe_get a (dn + c) -. (2. *. ai)))
      and lb =
        (Array.unsafe_get b (row + l) +. Array.unsafe_get b (row + rt) -. (2. *. bi))
        +. (ry *. (Array.unsafe_get b (up + c) +. Array.unsafe_get b (dn + c) -. (2. *. bi)))
      in
      let abb = ai *. bi *. bi in
      Array.unsafe_set a2 i (ai +. (diff_a *. la) -. abb +. (feed *. (1. -. ai)));
      Array.unsafe_set b2 i (bi +. (diff_b *. lb) +. abb -. ((kill +. feed) *. bi))
    done
  done;
  st.a <- a2; st.a2 <- a; st.b <- b2; st.b2 <- b

(* ---- Forces ---------------------------------------------------------- *)

(* Add a velocity field [f dx dy -> (fx, fy)] in a radius around (x, y). *)
let splat st x y radius f =
  let s = st.s in
  let c0 = int_of_float ((x -. radius) /. s.cw) and c1 = int_of_float ((x +. radius) /. s.cw) in
  let r0 = int_of_float ((y -. radius) /. s.ch) and r1 = int_of_float ((y +. radius) /. s.ch) in
  let inv = 1. /. (radius *. radius *. 0.25) in
  for r = r0 to r1 do
    for c = c0 to c1 do
      let dx = ((float_of_int c +. 0.5) *. s.cw) -. x and dy = ((float_of_int r +. 0.5) *. s.ch) -. y in
      let d2 = (dx *. dx) +. (dy *. dy) in
      if d2 < radius *. radius then begin
        let cc = ((c mod s.cols) + s.cols) mod s.cols and rr = ((r mod s.rows) + s.rows) mod s.rows in
        let i = (rr * s.cols) + cc in
        let w = Float.exp (-.d2 *. inv) in
        let fx, fy = f dx dy in
        st.vx.(i) <- st.vx.(i) +. (fx *. w);
        st.vy.(i) <- st.vy.(i) +. (fy *. w)
      end
    done
  done

let vortex st x y radius strength =
  splat st x y radius (fun dx dy -> (-.dy *. strength, dx *. strength))

let drop_ink st x y radius amount =
  let s = st.s in
  let c = int_of_float (x /. s.cw) and r = int_of_float (y /. s.ch) in
  let rc = int_of_float (radius /. s.cw) and rr = int_of_float (radius /. s.ch) in
  for dr = -rr to rr do
    for dc = -rc to rc do
      let fx = float_of_int dc /. float_of_int (max 1 rc) and fy = float_of_int dr /. float_of_int (max 1 rr) in
      let d2 = (fx *. fx) +. (fy *. fy) in
      if d2 < 1. then begin
        let cc = (((c + dc) mod s.cols) + s.cols) mod s.cols and rw = (((r + dr) mod s.rows) + s.rows) mod s.rows in
        let i = (rw * s.cols) + cc in
        st.ink.(i) <- Float.min 1.5 (st.ink.(i) +. (amount *. (1. -. d2)))
      end
    done
  done

let tap st now (x, y) =
  stats.taps <- stats.taps + 1;
  (* Vortex dipole travelling along the last pointer motion (or upward). *)
  let mx, my =
    let m = Float.sqrt ((!pdx *. !pdx) +. (!pdy *. !pdy)) in
    if m > 0.5 then (!pdx /. m, !pdy /. m) else (0., -1.)
  in
  let off = 26. in
  vortex st (x -. (my *. off)) (y +. (mx *. off)) 70. 0.11;
  vortex st (x +. (my *. off)) (y -. (mx *. off)) 70. (-0.11);
  drop_ink st x y 46. 1.2;
  let c = int_of_float (x /. st.s.cw) and r = int_of_float (y /. st.s.ch) in
  seed st c r;
  seed st (c + 5) (r + 2);
  seed st (c - 5) (r - 2);
  st.rings <- { rx = x; ry = y; born = now } :: st.rings

(* Two slow stirrers on Lissajous paths keep the field moving at rest. *)
let ambient st now =
  let t = now *. 0.00005 in
  let w = st.s.w and h = st.s.h in
  vortex st (w *. (0.5 +. (0.36 *. Float.sin (t *. 1.3)))) (h *. (0.5 +. (0.3 *. Float.sin (t *. 2.1)))) 220. 0.0016;
  vortex st (w *. (0.5 +. (0.4 *. Float.cos (t *. 0.9)))) (h *. (0.5 +. (0.34 *. Float.cos (t *. 1.7)))) 200. (-0.0016)

(* ---- Rendering ------------------------------------------------------- *)

let cell_ramp = Ascii.ramp ".:-=+*%#@"
let flow_glyphs = Ascii.ramp "-\\|/"
let data_glyphs = Ascii.ramp "01"
let ring_glyph = Ascii.glyph "o"
let dust = Ascii.glyph "."

let hash c r = ((c * 73856093) lxor (r * 19349663)) land 0xffff

let render st now =
  let s = st.s in
  let cols = s.cols and rows = s.rows in
  let lens = 130. in
  let covered = ref 0 in
  let tick = st.frame / 5 in
  for r = 0 to rows - 1 do
    let cy = (float_of_int r +. 0.5) *. s.ch in
    for c = 0 to cols - 1 do
      let i = (r * cols) + c in
      let bi = Array.unsafe_get st.b i and ink = Array.unsafe_get st.ink i in
      let vx = Array.unsafe_get st.vx i and vy = Array.unsafe_get st.vy i in
      let level = bi *. 3.2 in
      let g, band =
        if ink > 0.22 then
          (data_glyphs.((hash c r + tick) land 1), if ink > 0.55 then 3 else 2)
        else if level > 0.3 then begin
          (* Only the cores of the spots, so cells read as separate bodies. *)
          incr covered;
          (Ascii.pick cell_ramp ((level -. 0.3) /. 0.85), if level > 0.85 then 2 else 1)
        end
        else
          let sp = (vx *. vx) +. (vy *. vy) in
          if sp > 0.8 then begin
            (* Screen-space flow direction mapped to - \ | / *)
            let ang = Float.atan2 vy vx in
            let ang = if ang < 0. then ang +. Float.pi else ang in
            let k = int_of_float (Float.round (ang /. (Float.pi /. 4.))) land 3 in
            (flow_glyphs.(k), if sp > 4. then 1 else 0)
          end
          else if hash c r mod 97 = 0 then (dust, 0)
          else (0, 0)
      in
      let band =
        if g <> 0 && band < 2 && !pointer_in then begin
          let dx = ((float_of_int c +. 0.5) *. s.cw) -. !px and dy = cy -. !py in
          if (dx *. dx) +. (dy *. dy) < lens *. lens then band + 1 else band
        end
        else band
      in
      Array.unsafe_set s.glyphs i g;
      Array.unsafe_set s.band i band
    done
  done;
  (* Shock rings: expanding, thinning circles of accent glyphs. *)
  st.rings <- List.filter (fun rg -> now -. rg.born < 1400.) st.rings;
  List.iter
    (fun rg ->
      let age = now -. rg.born in
      let rad = 12. +. (age *. 0.32) in
      let steps = int_of_float (rad *. 6.283 /. s.cw) + 8 in
      let band = if age < 600. then 3 else 2 in
      for k = 0 to steps - 1 do
        if age < 900. || k land 1 = 0 then begin
          let th = float_of_int k *. 6.2832 /. float_of_int steps in
          let x = rg.rx +. (rad *. Float.cos th) and y = rg.ry +. (rad *. Float.sin th) in
          Ascii.put s (int_of_float (x /. s.cw)) (int_of_float (y /. s.ch)) ring_glyph band
        end
      done)
    st.rings;
  if st.frame mod 30 = 0 then stats.cover <- float_of_int !covered /. float_of_int (max 1 st.n);
  Ascii.flush s

(* ---- Step ------------------------------------------------------------ *)

let step st now dt =
  if Ascii.fit st.s then alloc st;
  if st.n > 0 then begin
    if st.frame = 0 then scatter st (max 6 (st.n / 260));
    st.frame <- st.frame + 1;
    stats.fps <- (stats.fps *. 0.95) +. (0.05 *. (1000. /. dt));
    let k = dt /. 16.7 in
    ambient st now;
    if !pointer_in && (Float.abs !pdx +. Float.abs !pdy) > 0. then begin
      splat st !px !py 70. (fun _ _ -> (!pdx *. 0.22, !pdy *. 0.22));
      pdx := !pdx *. 0.5;
      pdy := !pdy *. 0.5
    end;
    List.iter (tap st now) !taps;
    taps := [];
    (* Velocity: self-advect, damp, project. *)
    advect st k st.vx st.tx;
    advect st k st.vy st.ty;
    let ox = st.vx and oy = st.vy in
    st.vx <- st.tx; st.vy <- st.ty; st.tx <- ox; st.ty <- oy;
    let damp = 0.985 ** k in
    for i = 0 to st.n - 1 do
      Array.unsafe_set st.vx i (Array.unsafe_get st.vx i *. damp);
      Array.unsafe_set st.vy i (Array.unsafe_get st.vy i *. damp)
    done;
    project st 10;
    (* Scalars ride the flow. *)
    advect st k st.a st.a2;
    advect st k st.b st.b2;
    advect st k st.ink st.ink2;
    let t = st.a in st.a <- st.a2; st.a2 <- t;
    let t = st.b in st.b <- st.b2; st.b2 <- t;
    let t = st.ink in st.ink <- st.ink2; st.ink2 <- t;
    let decay = 0.965 ** k in
    for i = 0 to st.n - 1 do
      Array.unsafe_set st.ink i (Array.unsafe_get st.ink i *. decay)
    done;
    st.drift <- st.drift *. (0.9 ** k);
    (* Chemistry: fast-forward while booting, then a steady pace. *)
    let reps = if st.boot > 0 then (st.boot <- st.boot - 1; 24) else if st.n > 16000 then 3 else 5 in
    for _ = 1 to reps do react st done;
    if st.frame mod 240 = 0 && stats.cover < 0.02 then scatter st (max 4 (st.n / 500));
    render st now
  end

(* ---- Wiring ---------------------------------------------------------- *)

let start () =
  let small = Jv.to_float (Jv.get Jv.global "innerWidth") < 640. in
  let s =
    Ascii.create ~at:[ ("id", "field") ] ~fit:Ascii.Window ~palette:"--field"
      ~font_px:(if small then 11. else 13.) ~line:1.2 ()
  in
  El.append_children (Document.body G.document) [ Ascii.el s ];
  let st =
    { s; n = 0; vx = [||]; vy = [||]; tx = [||]; ty = [||]; p = [||]; dv = [||];
      a = [||]; b = [||]; a2 = [||]; b2 = [||]; ink = [||]; ink2 = [||];
      boot = 70; frame = 0; drift = 0.; rings = [] }
  in
  let win = Window.as_target G.window in
  let pos ev =
    let m = Ev.as_type ev in
    (Ev.Pointer.as_mouse m |> Ev.Mouse.client_x, Ev.Pointer.as_mouse m |> Ev.Mouse.client_y)
  in
  ignore (Ev.listen Ev.pointermove (fun ev ->
    let x, y = pos ev in
    if !pointer_in then begin
      pdx := !pdx +. (x -. !px);
      pdy := !pdy +. (y -. !py)
    end;
    px := x; py := y; pointer_in := true) win);
  ignore (Ev.listen Ev.pointerdown (fun ev ->
    let x, y = pos ev in
    px := x; py := y; pointer_in := true;
    taps := (x, y) :: !taps) win);
  (* Touch has no hover: let go of the lens once the finger lifts. *)
  ignore (Ev.listen Ev.pointerup (fun ev ->
    if Jstr.to_string (Ev.Pointer.type' (Ev.as_type ev)) <> "mouse" then pointer_in := false) win);
  ignore (Ev.listen Ev.mouseleave (fun _ -> pointer_in := false) (El.as_target (Document.root G.document)));
  let last_scroll = ref (Jv.to_float (Jv.get Jv.global "scrollY")) in
  ignore (Ev.listen Ev.scroll (fun _ ->
    let y = Jv.to_float (Jv.get Jv.global "scrollY") in
    st.drift <- Float.max (-12.) (Float.min 12. (st.drift -. ((y -. !last_scroll) *. 0.06)));
    last_scroll := y) win);
  Ascii.Ticker.add (step st)
