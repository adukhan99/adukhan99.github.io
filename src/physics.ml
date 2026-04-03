open Brr
open Brr_canvas
open Jv

let create_canvas () =
  let canvas = Canvas.create [] in
  let el = Canvas.to_el canvas in
  let jv_el = El.to_jv el in
  set jv_el "id" (of_string "bg-canvas");
  let style_jv = get jv_el "style" in
  set style_jv "position" (of_string "fixed");
  set style_jv "top" (of_string "0");
  set style_jv "left" (of_string "0");
  set style_jv "width" (of_string "100%");
  set style_jv "height" (of_string "100%");
  set style_jv "zIndex" (of_string "0");
  set style_jv "pointerEvents" (of_string "none");
  let body = Document.body G.document in
  El.append_children body [el];
  canvas

let setup_canvas canvas =
  let resize () =
    let win = Id.to_jv G.window in
    let w = to_int (get win "innerWidth") in
    let h = to_int (get win "innerHeight") in
    Canvas.set_w canvas w;
    Canvas.set_h canvas h;
  in
  resize ();
  ignore (Ev.listen Ev.resize (fun _ -> resize ()) (Window.as_target G.window));
  let ctx = C2d.get_context canvas in
  (ctx, resize)

let draw_status canvas ctx msg =
  let w = float_of_int (Canvas.w canvas) in
  let h = float_of_int (Canvas.h canvas) in
  C2d.clear_rect ctx ~x:0. ~y:0. ~w ~h;
  C2d.set_fill_style ctx (C2d.color (Jv.to_jstr (Jv.of_string "rgba(120, 120, 120, 0.9)")));
  C2d.set_font ctx (Jv.to_jstr (Jv.of_string "13px Raleway, sans-serif"));
  C2d.fill_text ctx (Jv.to_jstr (Jv.of_string msg)) ~x:18. ~y:28.

let start_with_rapier rapier canvas ctx =
    let gravity = obj [|("x", of_float 0.0); ("y", of_float 0.0)|] in
    let world = new' (get rapier "World") [| gravity |] in

    let num_particles = 80 in
    let particles = ref [] in

    let mouse_pos = ref None in
    ignore (Ev.listen Ev.mousemove (fun ev ->
      let mev = Ev.as_type ev in
      let x = Ev.Mouse.client_x mev in
      let y = Ev.Mouse.client_y mev in
      mouse_pos := Some (x, y)
    ) (Window.as_target G.window));
    
    ignore (Ev.listen Ev.mouseout (fun _ ->
      mouse_pos := None
    ) (Window.as_target G.window));

    for _ = 0 to num_particles - 1 do
      let r_desc = call (get rapier "RigidBodyDesc") "dynamic" [||] in
      let x = Random.float (float_of_int (Canvas.w canvas)) in
      let y = Random.float (float_of_int (Canvas.h canvas)) in
      ignore (call r_desc "setTranslation" [| of_float x; of_float y |]);
      ignore (call r_desc "setLinearDamping" [| of_float 0.5 |]);
      
      let body = call world "createRigidBody" [| r_desc |] in
      
      let c_desc = call (get rapier "ColliderDesc") "ball" [| of_float 4.0 |] in
      ignore (call world "createCollider" [| c_desc; body |]);
      
      particles := body :: !particles;
    done;

    let particle_array = Array.of_list !particles in
    let bonds = ref [] in

    for i = 0 to num_particles - 1 do
      for _ = 1 to 2 do
         let j = Random.int num_particles in
         if i <> j then begin
           bonds := (i, j) :: !bonds;
           let p1 = particle_array.(i) in
           let p2 = particle_array.(j) in
           let anchor = obj [|("x", of_float 0.0); ("y", of_float 0.0)|] in
           let params = call (get rapier "JointData") "spring" [| 
              of_float 80.0;
              of_float 1.0;
              of_float 0.1;
              anchor;
              anchor
           |] in
           ignore (call world "createImpulseJoint" [| params; p1; p2; of_bool true |])
         end
      done
    done;

    let positions = Array.make num_particles (0.0, 0.0) in

    let rec step _ =
      ignore (call world "step" [||]);
      let w = float_of_int (Canvas.w canvas) in
      let h = float_of_int (Canvas.h canvas) in
      C2d.clear_rect ctx ~x:0. ~y:0. ~w ~h;

      let root = Document.root G.document in
      let jv_root = El.to_jv root in
      let theme_val = call jv_root "getAttribute" [| of_string "data-theme" |] in
      let is_dark = if is_none theme_val then false else to_string theme_val = "dark" in
      
      let color_rgb = if is_dark then "150, 200, 255" else "50, 100, 200" in
      C2d.set_fill_style ctx (C2d.color (Jv.to_jstr (Jv.of_string ("rgba(" ^ color_rgb ^ ", 0.8)"))));
      C2d.set_stroke_style ctx (C2d.color (Jv.to_jstr (Jv.of_string ("rgba(" ^ color_rgb ^ ", 0.2)"))));
      C2d.set_line_width ctx 1.5;

      for i = 0 to num_particles - 1 do
        let body = particle_array.(i) in
        let pos = call body "translation" [||] in
        let x = to_float (get pos "x") in
        let y = to_float (get pos "y") in
        positions.(i) <- (x, y);

        let dx_center = (w /. 2.0) -. x in
        let dy_center = (h /. 2.0) -. y in
        let mut_fx = ref (dx_center *. 0.0005) in
        let mut_fy = ref (dy_center *. 0.0005) in

        (match !mouse_pos with
         | Some (mx, my) ->
             let dx_mouse = x -. mx in
             let dy_mouse = y -. my in
             let dsq = (dx_mouse ** 2.0) +. (dy_mouse ** 2.0) in
             if dsq < 40000.0 && dsq > 1.0 then begin
               let dist = sqrt dsq in
               let repel = 1000.0 /. dist in 
               mut_fx := !mut_fx +. ((dx_mouse /. dist) *. repel);
               mut_fy := !mut_fy +. ((dy_mouse /. dist) *. repel);
             end
         | None -> ());

        let impulse = obj [|("x", of_float !mut_fx); ("y", of_float !mut_fy)|] in
        ignore (call body "applyImpulse" [| impulse; of_bool true |]);

        let path = C2d.Path.create () in
        C2d.Path.arc path ~cx:x ~cy:y ~r:3.0 ~start:0.0 ~stop:(2.0 *. 3.14159);
        C2d.fill ctx path;
      done;

      List.iter (fun (i, j) ->
        let (x1, y1) = positions.(i) in
        let (x2, y2) = positions.(j) in
        let dsq = ((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0) in
        if dsq < 40000.0 then begin
           let path = C2d.Path.create () in
           C2d.Path.move_to path ~x:x1 ~y:y1;
           C2d.Path.line_to path ~x:x2 ~y:y2;
           C2d.stroke ctx path;
        end
      ) !bonds;

      ignore (G.request_animation_frame step);
      ()
    in
    ignore (G.request_animation_frame step);
    ()

let start () =
  let canvas = create_canvas () in
  let (ctx, _resize) = setup_canvas canvas in
  draw_status canvas ctx "Loading physics background...";
  let rapier = get global "RAPIER" in
  if Jv.is_none rapier then (
    Console.warn [Jv.to_jstr (Jv.of_string "RAPIER global not found; physics background disabled.")];
    draw_status canvas ctx "Physics background disabled (RAPIER global missing)."
  ) else
    let init = get rapier "init" in
    if Jv.is_none init then (
      start_with_rapier rapier canvas ctx
    ) else (
      let p = call rapier "init" [||] in
      ignore (call p "then" [| repr (fun _ -> start_with_rapier rapier canvas ctx) |]);
      ignore (call p "catch" [| repr (fun err ->
        Console.warn [
          Jv.to_jstr (Jv.of_string "RAPIER.init() failed; physics background disabled.");
          Jv.to_jstr err
        ];
        draw_status canvas ctx "Physics background disabled (RAPIER init failed).";
      ) |])
    )
