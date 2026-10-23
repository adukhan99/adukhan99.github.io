(* scene.ml — tiny 3D rasteriser onto an Ascii surface, and the project
   visuals built with it.

   World space is roughly the unit cube, y up, z toward the viewer. Points
   are rotated by the camera (yaw, pitch), perspective-projected to css px,
   then binned into cells with a depth test. Everything is drawn from
   primitives — points, lines, shaded spheres — so a new visual is just a
   function from (camera, time) to a handful of those calls. *)

type cam = {
  s : Ascii.t;
  yaw : float;
  pitch : float;
  scale : float;                    (* px per world unit *)
}

let fov = 3.2

(* Rotate into camera space. *)
let view cam (x, y, z) =
  let cy = Float.cos cam.yaw and sy = Float.sin cam.yaw in
  let x1 = (x *. cy) +. (z *. sy) and z1 = (z *. cy) -. (x *. sy) in
  let cp = Float.cos cam.pitch and sp = Float.sin cam.pitch in
  let y2 = (y *. cp) -. (z1 *. sp) and z2 = (z1 *. cp) +. (y *. sp) in
  (x1, y2, z2)

(* Camera space -> (px, py, depth, perspective factor). *)
let screen cam (x, y, z) =
  let k = fov /. (fov -. z) in
  ((cam.s.Ascii.w /. 2.) +. (x *. cam.scale *. k), (cam.s.Ascii.h /. 2.) -. (y *. cam.scale *. k), z, k)

let cell cam sx sy = (int_of_float (sx /. cam.s.Ascii.cw), int_of_float (sy /. cam.s.Ascii.ch))

(* Shading ramp, dim to bright (a nod to donut.c). *)
let shade = Ascii.ramp ".,-~:;=!*#$@"
let depth_ramp = Ascii.ramp ".:+o*O#@"
let slope_glyphs = Ascii.ramp "-\\|/"

(* Depth cue: map camera z in [-1, 1] to [0, 1]. *)
let near z = Float.max 0. (Float.min 1. ((z +. 1.1) /. 2.2))

let point cam ?glyph p band =
  let sx, sy, z, _ = screen cam (view cam p) in
  let c, r = cell cam sx sy in
  let g = match glyph with Some g -> g | None -> Ascii.pick depth_ramp (near z) in
  Ascii.put_z cam.s c r z g band

(* A line whose glyph follows its on-screen slope. *)
let line cam ?glyph ?(dotted = false) p q band =
  let ax, ay, az, _ = screen cam (view cam p) and bx, by, bz, _ = screen cam (view cam q) in
  let g =
    match glyph with
    | Some g -> g
    | None ->
      let ang = Float.atan2 (by -. ay) (bx -. ax) in
      let ang = if ang < 0. then ang +. Float.pi else ang in
      slope_glyphs.(int_of_float (Float.round (ang /. (Float.pi /. 4.))) land 3)
  in
  let dc = Float.abs (bx -. ax) /. cam.s.Ascii.cw and dr = Float.abs (by -. ay) /. cam.s.Ascii.ch in
  let n = int_of_float (Float.max dc dr) + 1 in
  for i = 0 to n do
    if (not dotted) || i land 1 = 0 then begin
      let t = float_of_int i /. float_of_int n in
      let c, r = cell cam (ax +. ((bx -. ax) *. t)) (ay +. ((by -. ay) *. t)) in
      (* Lines sit a hair behind surfaces at the same depth. *)
      Ascii.put_z cam.s c r (az +. ((bz -. az) *. t) -. 0.02) g band
    end
  done

let light = let l = (-0.45, 0.6, 0.66) in let x, y, z = l in let m = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z)) in (x /. m, y /. m, z /. m)

(* A Lambert-shaded sphere impostor. *)
let sphere cam p radius band =
  let sx, sy, z, k = screen cam (view cam p) in
  let pr = radius *. cam.scale *. k in
  let s = cam.s in
  let c0, r0 = cell cam (sx -. pr) (sy -. pr) and c1, r1 = cell cam (sx +. pr) (sy +. pr) in
  let lx, ly, lz = light in
  for r = r0 to r1 do
    for c = c0 to c1 do
      let dx = (((float_of_int c +. 0.5) *. s.Ascii.cw) -. sx) /. pr
      and dy = (((float_of_int r +. 0.5) *. s.Ascii.ch) -. sy) /. pr in
      let d2 = (dx *. dx) +. (dy *. dy) in
      if d2 < 1. then begin
        let nz = Float.sqrt (1. -. d2) in
        let lum = Float.max 0. ((dx *. lx) -. (dy *. ly) +. (nz *. lz)) in
        Ascii.put_z s c r (z +. (radius *. nz)) (Ascii.pick shade (0.08 +. (lum *. 0.92))) band
      end
    done
  done

(* ---- Visuals --------------------------------------------------------- *)

type visual =
  | Dna_bound          (* bent double helix with two bound proteins *)
  | Agent_graph        (* orchestrator hub, agent nodes, travelling messages *)
  | Torus
  | Wireframe of [ `Cube | `Octahedron | `Icosahedron ]

let golden = (1. +. Float.sqrt 5.) /. 2.

let polyhedron = function
  | `Cube ->
    let v = Array.init 8 (fun i ->
      let b k = if i land k <> 0 then 0.6 else -0.6 in (b 1, b 2, b 4)) in
    (v, 1.2)
  | `Octahedron ->
    ([| (0.9, 0., 0.); (-0.9, 0., 0.); (0., 0.9, 0.); (0., -0.9, 0.); (0., 0., 0.9); (0., 0., -0.9) |], 1.28)
  | `Icosahedron ->
    let k = 0.82 /. Float.sqrt (1. +. (golden *. golden)) in
    let v = ref [] in
    List.iter (fun (a, b) ->
      v := (0., a *. k, b *. golden *. k) :: (a *. k, b *. golden *. k, 0.) :: (b *. golden *. k, 0., a *. k) :: !v)
      [ (1., 1.); (1., -1.); (-1., 1.); (-1., -1.) ];
    (Array.of_list !v, 2. *. k)

(* Edges are vertex pairs at the shortest separation. *)
let edges_of (v, len) =
  let d (x, y, z) (a, b, c) = Float.sqrt (((x -. a) ** 2.) +. ((y -. b) ** 2.) +. ((z -. c) ** 2.)) in
  let es = ref [] in
  Array.iteri (fun i p -> Array.iteri (fun j q -> if i < j && Float.abs (d p q -. len) < 0.05 then es := (i, j) :: !es) v) v;
  Array.of_list !es

(* -- DNA with nucleoid-associated proteins ------------------------------ *)

let rung = Ascii.glyph ":"

let dna cam t =
  let r = 0.24 and pitch = 0.78 in
  let s1 = -0.62 and s2 = 0.62 and w = 0.36 and bend = 0.3 in
  (* Centreline bends over each protein, in opposite directions. *)
  let cy s = (bend *. Float.exp (-.(((s -. s1) /. w) ** 2.))) -. (bend *. Float.exp (-.(((s -. s2) /. w) ** 2.))) in
  let dcy s = (cy (s +. 0.001) -. cy (s -. 0.001)) /. 0.002 in
  let strand s phase =
    let d = dcy s in
    let m = Float.sqrt (1. +. (d *. d)) in
    let nx = -.d /. m and ny = 1. /. m in
    let ph = (6.2832 *. s /. pitch) +. phase +. (t *. 0.0011) in
    (s +. (r *. Float.cos ph *. nx), cy s +. (r *. Float.cos ph *. ny), r *. Float.sin ph)
  in
  let len = 1.5 in
  let n = 150 in
  let prev = ref None in
  for i = 0 to n do
    let sv = -.len +. (2. *. len *. float_of_int i /. float_of_int n) in
    (* Backbones as continuous strokes whose glyphs follow the slope; the
       half facing away is drawn dimmer, so the twist reads in depth. *)
    let p1 = strand sv 0. and p2 = strand sv 2.2 in
    (match !prev with
     | Some (q1, q2) ->
       let band p = let _, _, z = view cam p in if z < 0. then 0 else 2 in
       line cam q1 p1 (band p1);
       line cam q2 p2 (min 1 (band p2))
     | None -> ());
    prev := Some (p1, p2);
    if i mod 5 = 0 then line cam ~glyph:rung p1 p2 0
  done;
  (* Proteins: lumpy clusters seated on the concave side of each bend. *)
  let blob (x, y, z) dir phase =
    let br = 1. +. (0.04 *. Float.sin ((t *. 0.002) +. phase)) in
    List.iter (fun (dx, dy, dz, rad) -> sphere cam (x +. dx, y +. (dir *. dy), z +. dz) (rad *. br) 3)
      [ (0., 0., 0., 0.17); (0.13, 0.1, 0.06, 0.13); (-0.12, 0.13, -0.05, 0.12); (0.03, 0.23, 0.02, 0.1) ]
  in
  blob (s1, cy s1 -. 0.44, 0.) (-1.) 0.;
  blob (s2, cy s2 +. 0.44, 0.) 1. 2.

(* -- Agent graph ------------------------------------------------------- *)

type msg = { mutable src : int; mutable dst : int; mutable prev : int; mutable u : float; speed : float }

let agent_state = lazy (
  let ico = polyhedron `Icosahedron in
  let verts, _ = ico in
  let es = edges_of ico in
  let nbrs = Array.make (Array.length verts) [] in
  Array.iter (fun (i, j) -> nbrs.(i) <- j :: nbrs.(i); nbrs.(j) <- i :: nbrs.(j)) es;
  let msgs = Array.init 7 (fun k ->
    let src = k mod Array.length verts in
    { src; dst = List.hd nbrs.(src); prev = -1; u = Random.float 1.; speed = 0.0005 +. Random.float 0.0004 }) in
  (verts, es, nbrs, msgs))

let hub_glyph = Ascii.glyph "@"
let msg_trail = Ascii.ramp ".+*"

let agents cam _t dt =
  let verts, es, nbrs, msgs = Lazy.force agent_state in
  let lerp (ax, ay, az) (bx, by, bz) u = (ax +. ((bx -. ax) *. u), ay +. ((by -. ay) *. u), az +. ((bz -. az) *. u)) in
  Array.iter (fun (i, j) -> line cam verts.(i) verts.(j) 0) es;
  (* The orchestrator, with dotted dispatch lines to each busy agent. *)
  sphere cam (0., 0., 0.) 0.2 3;
  Array.iter (fun v -> sphere cam v 0.075 2) verts;
  Array.iter (fun m ->
    m.u <- m.u +. (m.speed *. dt);
    if m.u >= 1. then begin
      m.u <- 0.;
      let choices = List.filter (fun j -> j <> m.src) nbrs.(m.dst) in
      let next = List.nth choices (Random.int (List.length choices)) in
      m.prev <- m.src; m.src <- m.dst; m.dst <- next
    end;
    let a = verts.(m.src) and b = verts.(m.dst) in
    line cam ~dotted:true (0., 0., 0.) a 1;
    for k = 0 to 2 do
      let u = m.u -. (float_of_int (2 - k) *. 0.07) in
      if u > 0. then point cam ~glyph:msg_trail.(k) (lerp a b u) 3
    done;
    point cam ~glyph:hub_glyph (lerp a b m.u) 3)
    msgs

(* -- Torus ------------------------------------------------------------- *)

let torus cam t =
  let rr = 0.62 and r = 0.26 in
  let lx, ly, lz = light in
  let spin = t *. 0.0004 in
  for i = 0 to 90 do
    let th = float_of_int i *. 6.2832 /. 90. in
    for j = 0 to 34 do
      let ph = (float_of_int j *. 6.2832 /. 34.) +. spin in
      let cx = (rr +. (r *. Float.cos ph)) *. Float.cos th and cz = (rr +. (r *. Float.cos ph)) *. Float.sin th in
      let p = (cx, r *. Float.sin ph, cz) in
      let n = view cam (Float.cos ph *. Float.cos th, Float.sin ph, Float.cos ph *. Float.sin th) in
      let nx, ny, nz = n in
      let lum = Float.max 0. ((nx *. lx) +. (ny *. ly) +. (nz *. lz)) in
      let sx, sy, z, _ = screen cam (view cam p) in
      let c, rw = cell cam sx sy in
      Ascii.put_z cam.s c rw z (Ascii.pick shade (0.05 +. (lum *. 0.95))) (if lum > 0.7 then 2 else 1)
    done
  done

let wireframe cam shape =
  let v, _ as poly = polyhedron shape in
  Array.iter (fun (i, j) -> line cam v.(i) v.(j) 1) (edges_of poly);
  Array.iter (fun p -> sphere cam p 0.07 3) v

(* ---- Driver ---------------------------------------------------------- *)

type mount = {
  surface : Ascii.t;
  visual : visual;
  mutable visible : bool;
  mutable hover : float;            (* 0 idle .. 1 hovered, eased *)
  mutable tx : float; mutable ty : float;   (* pointer target, -1..1 *)
  mutable ox : float; mutable oy : float;   (* eased pointer offset *)
  mutable clock : float;            (* local time, runs faster on hover *)
  mutable hovering : bool;
}

let draw m dt =
  let s = m.surface in
  ignore (Ascii.fit s);
  if s.Ascii.cols > 0 then begin
    let e = 0.08 in
    m.hover <- m.hover +. (((if m.hovering then 1. else 0.) -. m.hover) *. e);
    m.ox <- m.ox +. ((m.tx -. m.ox) *. e);
    m.oy <- m.oy +. ((m.ty -. m.oy) *. e);
    let dt = dt *. (1. +. (1.6 *. m.hover)) in
    m.clock <- m.clock +. dt;
    let t = m.clock in
    let base = Float.min s.Ascii.w (s.Ascii.h *. 2.2) in
    let yaw, pitch, scale =
      match m.visual with
      | Dna_bound -> (0.18 *. Float.sin (t *. 0.00023), 0.14, s.Ascii.w *. 0.31)
      | Agent_graph -> (t *. 0.00028, 0.32, base *. 0.36)
      | Torus -> (t *. 0.0003, 0.9 +. (0.2 *. Float.sin (t *. 0.0004)), base *. 0.42)
      | Wireframe _ -> (t *. 0.0004, 0.45 +. (0.25 *. Float.sin (t *. 0.0003)), base *. 0.38)
    in
    let scale = Float.min scale (s.Ascii.h *. 0.62) in
    let cam = { s; yaw = yaw +. (m.ox *. 0.7); pitch = pitch +. (m.oy *. 0.45); scale } in
    Ascii.clear s;
    (match m.visual with
     | Dna_bound -> dna cam t
     | Agent_graph -> agents cam t dt
     | Torus -> torus cam t
     | Wireframe sh -> wireframe cam sh);
    Ascii.flush s
  end

let mount ~host ?(track = host) visual =
  let surface = Ascii.create ~fit:Ascii.Element ~palette:"--glyph" ~font_px:11. ~line:1.15 () in
  Brr.El.append_children host [ Ascii.el surface ];
  let m = { surface; visual; visible = false; hover = 0.; tx = 0.; ty = 0.; ox = 0.; oy = 0.;
            clock = Random.float 20000.; hovering = false } in
  Ascii.on_visibility host (fun v -> m.visible <- v);
  let open Brr in
  let target = El.as_target track in
  ignore (Ev.listen Ev.pointermove (fun ev ->
    let mev = Ev.Pointer.as_mouse (Ev.as_type ev) in
    let r = El.to_jv track in
    let rect = Jv.call r "getBoundingClientRect" [||] in
    let f k = Jv.to_float (Jv.get rect k) in
    m.tx <- (((Ev.Mouse.client_x mev -. f "left") /. f "width") *. 2.) -. 1.;
    m.ty <- (((Ev.Mouse.client_y mev -. f "top") /. f "height") *. 2.) -. 1.;
    m.hovering <- true) target);
  ignore (Ev.listen Ev.pointerleave (fun _ -> m.hovering <- false; m.tx <- 0.; m.ty <- 0.) target);
  draw m 16.;
  Ascii.Ticker.add (fun _ dt -> if m.visible then draw m dt);
  m
