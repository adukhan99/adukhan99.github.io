open Brr

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
let hidden = ("aria-hidden", "true")
let external_ = [ ("target", "_blank"); ("rel", "noopener noreferrer") ]

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
let pre ?at ?inner () = el "pre" ?at ?inner ()
let aside ?at ?inner () = el "aside" ?at ?inner ()
let dl ?at ?inner () = el "dl" ?at ?inner ()
let dt ?at ?inner () = el "dt" ?at ?inner ()
let dd ?at ?inner () = el "dd" ?at ?inner ()

(* Component Helpers *)

(* A Copland-style window: title bar, decorative controls, body. *)
let window ?(at = []) ?(extra = "") ~title ?(sub = "") inner =
  section ~at:(cls ("win " ^ extra) :: at) ~inner:[
    div ~at:[ cls "win-bar" ] ~inner:[
      span ~at:[ cls "win-title" ] ~inner:[ txt title ] ();
      span ~at:[ cls "win-sub" ] ~inner:[ txt sub ] ();
      span ~at:[ cls "win-ctl"; hidden ] ~inner:[ txt "_ \xe2\x96\xa1 \xc3\x97" ] ()
    ] ();
    div ~at:[ cls "win-body" ] ~inner ()
  ] ()

(* Section heading: index, title and an optional subtitle. *)
let section_head ~index ~title ?sub () =
  el "header" ~at:[ cls "section-head" ] ~inner:([
    p ~at:[ cls "section-index"; hidden ] ~inner:[ txt ("// " ^ index) ] ();
    h2 ~inner:[ txt title ] ()
  ] @ match sub with Some s -> [ p ~at:[ cls "section-sub" ] ~inner:[ txt s ] () ] | None -> []) ()

let render_navbar ~links =
  let brand = a ~at:[ cls "brand"; href "./index.html"; aria_label "Alexander Dukhan, home" ] ~inner:[
    img ~at:[ ("src", "./assets/logo.png"); ("alt", ""); ("width", "28"); ("height", "28") ] ();
    span ~at:[ cls "brand-name" ] ~inner:[ txt "a.dukhan" ] ();
    span ~at:[ cls "brand-sub"; hidden ] ~inner:[ txt "//wired" ] ()
  ] () in
  let menu = ul ~at:[ cls "menu" ] ~inner:(List.map (fun (h, l) ->
    li ~inner:[ a ~at:[ cls "menu-link"; href h ] ~inner:[ txt l ] () ] ()) links
    @ [ li ~inner:[ a ~at:[ cls "menu-link"; href "#"; ("data-email-link", "true") ] ~inner:[ txt "mail" ] () ] () ]) () in
  let theme_switch = div ~at:[ cls "theme-switch" ] ~inner:[
    input ~at:[ ("type", "checkbox"); id_ "switch"; aria_label "Dark theme" ] ();
    label ~at:[ cls "theme-toggle"; ("for", "switch") ] ~inner:[
      span ~at:[ cls "t-dark" ] ~inner:[ txt "\xe2\x97\x90"; span ~at:[ cls "t-word" ] ~inner:[ txt " phosphor" ] () ] ();
      span ~at:[ cls "t-light" ] ~inner:[ txt "\xe2\x97\x91"; span ~at:[ cls "t-word" ] ~inner:[ txt " paper" ] () ] ()
    ] ()
  ] () in
  let clock = span ~at:[ cls "clock"; ("title", "Local time in York") ] ~inner:[
    span ~at:[ cls "clock-zone" ] ~inner:[ txt "YRK" ] ();
    span ~at:[ id_ "clock" ] ~inner:[ txt "--:--:--" ] ()
  ] () in
  el "nav" ~at:[ cls "navbar"; aria_label "Main" ] ~inner:[
    div ~at:[ cls "navbar-inner" ] ~inner:[
      brand; menu; div ~at:[ cls "navbar-right" ] ~inner:[ theme_switch; clock ] ()
    ] ()
  ] ()

(* 88x31-style buttons, the web's oldest badge format. *)
let badge ?(at = []) ~label:l ?(sub = "") ~style () =
  let tag = if List.mem_assoc "href" at then "a" else "span" in
  el tag ~at:(cls ("badge badge--" ^ style) :: at) ~inner:[
    span ~at:[ cls "badge-main" ] ~inner:[ txt l ] ();
    span ~at:[ cls "badge-sub" ] ~inner:[ txt sub ] ()
  ] ()

let render_footer () =
  footer ~at:[ id_ "footer" ] ~inner:[
    div ~at:[ cls "container footer-inner" ] ~inner:[
      a ~at:[ cls "footer-mail"; ("data-email-link", "true"); href "#" ] ~inner:[ txt "<< send me an email >>" ] ();
      div ~at:[ cls "badges" ] ~inner:[
        badge ~label:"OCAML" ~sub:"powered" ~style:"ocaml" ();
        badge ~label:"JSOO" ~sub:"compiled" ~style:"plain" ();
        badge ~label:"NO" ~sub:"cookies" ~style:"invert" ();
        badge ~label:"ANY" ~sub:"browser" ~style:"plain" ()
      ] ();
      p ~at:[ cls "colophon"; hidden ] ~inner:[ txt "no matter where you go, everyone's connected." ] ()
    ] ()
  ] ()
