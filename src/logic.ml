open Js_of_ocaml
open Js

module Style = Style
module Ui = Ui

let setup_logic () =
  let doc = Dom_html.document in
  let get_el sel = Opt.to_option (doc##querySelector (string sel)) in

  let hamburger = get_el ".hamburger" in
  let nav_menu = get_el ".nav-menu" in
  (match hamburger, nav_menu with
   | Some h, Some m ->
       let close () =
         h##.classList##remove (string "active");
         m##.classList##remove (string "active");
         h##setAttribute (string "aria-expanded") (string "false")
       in
       h##.onclick := Dom_html.handler (fun _ ->
         let active = h##.classList##toggle (string "active") in
         ignore (m##.classList##toggle (string "active"));
         h##setAttribute (string "aria-expanded") (string (string_of_bool (to_bool active)));
         _true
       );
       let links = doc##querySelectorAll (string ".nav-link") in
       for i = 0 to links##.length - 1 do
         Opt.iter (links##item i) (fun l -> l##.onclick := Dom_html.handler (fun _ -> close (); _true))
       done;
       doc##.onkeydown := Dom_html.handler (fun ev ->
         if to_string (Unsafe.get ev (string "key")) = "Escape" then close ();
         _true
       )
   | _ -> ());

  let switch = get_el ".theme-switch input[type=\"checkbox\"]" in
  let root = doc##.documentElement in
  (match switch with
   | Some s ->
       let s_in = Opt.get (Dom_html.CoerceTo.input s) (fun () -> failwith "not input") in
       let storage = Dom_html.window##.localStorage in
       Optdef.iter storage (fun st ->
         let theme_opt = Opt.to_option (st##getItem (string "theme")) in
         (match theme_opt with
          | Some t ->
              root##setAttribute (string "data-theme") t;
              s_in##.checked := bool (to_string t = "dark")
          | None -> ());
         s_in##.onchange := Dom_html.handler (fun _ ->
           let t = if to_bool s_in##.checked then "dark" else "light" in
           root##setAttribute (string "data-theme") (string t);
           st##setItem (string "theme") (string t);
           _true
         )
       )
   | None -> ());

  let email_links = doc##querySelectorAll (string "[data-email-link='true']") in
  let decoded = to_string (Dom_html.window##atob (string "YWdkdWtoYW5AZ21haWwuY29t")) in
  for i = 0 to email_links##.length - 1 do
    Opt.iter (email_links##item i) (fun l -> l##setAttribute (string "href") (string ("mailto:" ^ decoded)))
  done;

  let navbar = get_el ".navbar" in
  (match navbar with
   | Some n ->
       let set_state () =
         let scrolled = (Unsafe.coerce Dom_html.window)##.scrollY in
         if to_float scrolled > 8.0 then n##.classList##add (string "scrolled") else n##.classList##remove (string "scrolled")
       in
       set_state ();
       let opts = Unsafe.obj [|("passive", Unsafe.inject _true)|] in
       ignore (Unsafe.meth_call Dom_html.window "addEventListener" [| Unsafe.inject (string "scroll"); Unsafe.inject (wrap_callback (fun _ -> set_state ())); Unsafe.inject opts |])
   | None -> ())
