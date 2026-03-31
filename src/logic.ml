open Js_of_ocaml

let setup_logic () =
  let doc = Dom_html.document in
  let get_el sel = Js.Opt.to_option (doc##querySelector (Js.string sel)) in

  let hamburger = get_el ".hamburger" in
  let nav_menu = get_el ".nav-menu" in
  (match hamburger, nav_menu with
   | Some h, Some m ->
       let close () =
         h##.classList##remove (Js.string "active");
         m##.classList##remove (Js.string "active");
         h##setAttribute (Js.string "aria-expanded") (Js.string "false")
       in
       h##.onclick := Dom_html.handler (fun _ ->
         let active = h##.classList##toggle (Js.string "active") in
         ignore (m##.classList##toggle (Js.string "active"));
         h##setAttribute (Js.string "aria-expanded") (Js.string (string_of_bool (Js.to_bool active)));
         Js._true
       );
       let links = doc##querySelectorAll (Js.string ".nav-link") in
       for i = 0 to links##.length - 1 do
         Js.Opt.iter (links##item i) (fun l -> l##.onclick := Dom_html.handler (fun _ -> close (); Js._true))
       done;
       doc##.onkeydown := Dom_html.handler (fun ev ->
         if Js.to_string (Js.Unsafe.get ev (Js.string "key")) = "Escape" then close ();
         Js._true
       )
   | _ -> ());

  let switch = get_el ".theme-switch input[type=\"checkbox\"]" in
  let root = doc##.documentElement in
  (match switch with
   | Some s ->
       let s_in = Js.Opt.get (Dom_html.CoerceTo.input s) (fun () -> failwith "not input") in
       let storage = Dom_html.window##.localStorage in
       Js.Optdef.iter storage (fun st ->
         let theme_opt = Js.Opt.to_option (st##getItem (Js.string "theme")) in
         (match theme_opt with
          | Some t ->
              root##setAttribute (Js.string "data-theme") t;
              s_in##.checked := Js.bool (Js.to_string t = "dark")
          | None -> ());
         s_in##.onchange := Dom_html.handler (fun _ ->
           let t = if Js.to_bool s_in##.checked then "dark" else "light" in
           root##setAttribute (Js.string "data-theme") (Js.string t);
           st##setItem (Js.string "theme") (Js.string t);
           Js._true
         )
       )
   | None -> ());

  let email_links = doc##querySelectorAll (Js.string "[data-email-link='true']") in
  let decoded = Js.to_string (Dom_html.window##atob (Js.string "YWdkdWtoYW5AZ21haWwuY29t")) in
  for i = 0 to email_links##.length - 1 do
    Js.Opt.iter (email_links##item i) (fun l -> l##setAttribute (Js.string "href") (Js.string ("mailto:" ^ decoded)))
  done;

  let navbar = get_el ".navbar" in
  (match navbar with
   | Some n ->
       let set_state () =
         let scrolled = (Js.Unsafe.coerce Dom_html.window)##.scrollY in
         if Js.to_float scrolled > 8.0 then n##.classList##add (Js.string "scrolled") else n##.classList##remove (Js.string "scrolled")
       in
       set_state ();
       let opts = Js.Unsafe.obj [|("passive", Js.Unsafe.inject Js._true)|] in
       ignore (Js.Unsafe.meth_call Dom_html.window "addEventListener" [| Js.Unsafe.inject (Js.string "scroll"); Js.Unsafe.inject (Js.wrap_callback (fun _ -> set_state ())); Js.Unsafe.inject opts |])
   | None -> ())
