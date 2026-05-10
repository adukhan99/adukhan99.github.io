open Brr
open Js_of_ocaml

let el name ?(at = []) ?(inner = []) () =
  let e = El.v (Jstr.v name) inner in
  List.iter (fun (k, v) -> El.set_at (Jstr.v k) (Some (Jstr.v v)) e) at;
  e

let txt s = El.txt (Jstr.v s)

(* DSL Helpers *)
let cls s = ("class", s)
let id_ s = ("id", s)
let href s = ("href", s)
let aria_label s = ("aria-label", s)

let div ?at ?inner () = el "div" ?at ?inner ()
let span ?at ?inner () = el "span" ?at ?inner ()
let i ?at ?inner () = el "i" ?at ?inner ()
let p ?at ?inner () = el "p" ?at ?inner ()
let h1 ?at ?inner () = el "h1" ?at ?inner ()
let h2 ?at ?inner () = el "h2" ?at ?inner ()
let h3 ?at ?inner () = el "h3" ?at ?inner ()
let li ?at ?inner () = el "li" ?at ?inner ()
let a ?at ?inner () = el "a" ?at ?inner ()
let section ?at ?inner () = el "section" ?at ?inner ()
let img ?at () = el "img" ?at ()
let ul ?at ?inner () = el "ul" ?at ?inner ()
let button ?at ?inner () = el "button" ?at ?inner ()
let input ?at () = el "input" ?at ()
let label ?at ?inner () = el "label" ?at ?inner ()
let article ?at ?inner () = el "article" ?at ?inner ()
let footer ?at ?inner () = el "footer" ?at ?inner ()

(* Component Helpers *)
let nav_link ~href_url ~label ?(btn_cls = "btn-primary") ?icon () =
  let content = match icon with
    | Some i_cls -> [txt (label ^ " "); i ~at:[cls i_cls] ()]
    | None -> [txt label]
  in
  li ~inner:[a ~at:[cls ("nav-link btn " ^ btn_cls); href href_url] ~inner:content ()] ()

let render_navbar ~links =
  let logo = h1 ~at:[id_ "logo"] ~inner:[
    a ~at:[href "./index.html"] ~inner:[
      img ~at:[("src", "./assets/logo.png"); ("alt", "Alexander Dukhan logo"); ("loading", "lazy")] ()
    ] ()
  ] () in

  let nav_links = List.map (fun (h, l, b, i) -> nav_link ~href_url:h ~label:l ~btn_cls:b ?icon:i ()) links in
  
  let theme_switch = li ~at:[cls "theme-switch-wrap"] ~inner:[
    div ~at:[cls "theme-switch"] ~inner:[
      input ~at:[("type", "checkbox"); id_ "switch"; aria_label "Toggle color theme"] ();
      label ~at:[cls "toggle-icons"; ("for", "switch")] ~inner:[
        img ~at:[cls "moon"; ("src", "./assets/moon.svg"); ("alt", "Dark mode"); ("loading", "lazy")] ();
        img ~at:[cls "sun"; ("src", "./assets/sun.svg"); ("alt", "Light mode"); ("loading", "lazy")] ()
      ] ()
    ] ()
  ] () in

  let nav = el "nav" ~at:[cls "navbar"] ~inner:[
    div ~at:[cls "container"] ~inner:[
      logo;
      ul ~at:[cls "nav-menu"] ~inner:(nav_links @ [theme_switch]);
      button ~at:[cls "hamburger"; ("type", "button"); aria_label "Toggle menu"; ("aria-expanded", "false")]
        ~inner:[span ~at:[cls "bar"] (); span ~at:[cls "bar"] (); span ~at:[cls "bar"] ()] ()
    ] ()
  ] () in
  nav

let render_footer () =
  footer ~at:[id_ "footer"] ~inner:[
    div ~at:[cls "container"] ~inner:[
      a ~at:[("data-email-link", "true"); href "#"] ~inner:[txt "Send me an Email"] ()
    ] ()
  ] ()
