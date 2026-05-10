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

let project_card ?link ~title ~desc ~status ~card_cls () =
  let bio = [
    h3 ~inner:[txt title] ();
    p ~inner:[txt desc] ();
    p ~at:[cls "project-status"] ~inner:[txt status] ()
  ] in
  let info_inner = match link with
    | Some l -> [
        div ~at:[cls "project-bio"] ~inner:bio ();
        div ~at:[cls "project-link"] ~inner:[
          a ~at:[href l; ("target", "_blank"); ("rel", "noopener noreferrer"); aria_label ("Open " ^ title ^ " on GitHub")]
            ~inner:[i ~at:[cls "fas fa-globe"] ()] ()
        ] ()
      ]
    | None -> [div ~at:[cls "project-bio"] ~inner:bio ()]
  in
  section ~at:[cls ("card project-card " ^ card_cls)] ~inner:[
    div ~at:[cls "project-info"] ~inner:info_inner ()
  ] ()

let social_link ~url ~aria ?img_src ?icon_cls () =
  let inner = match icon_cls, img_src with
    | Some c, _ -> [i ~at:[cls c] ()]
    | _, Some src -> [img ~at:[("src", src); ("alt", aria); ("loading", "lazy")] ()]
    | _ -> []
  in
  a ~at:[href url; ("target", "_blank"); ("rel", "noopener noreferrer"); aria_label aria] ~inner ()

let render_hero () =
  let logo = h1 ~at:[id_ "logo"] ~inner:[
    a ~at:[href "./index.html"] ~inner:[
      img ~at:[("src", "./assets/logo.png"); ("alt", "Alexander Dukhan logo"); ("loading", "lazy")] ()
    ] ()
  ] () in

  let nav_links = [
    nav_link ~href_url:"./resume.html" ~label:"Resume" ~icon:"fas fa-arrow-right" ();
    nav_link ~href_url:"#projects" ~label:"Research" ~btn_cls:"btn-outline" ();
    li ~at:[cls "theme-switch-wrap"] ~inner:[
      div ~at:[cls "theme-switch"] ~inner:[
        input ~at:[("type", "checkbox"); id_ "switch"; aria_label "Toggle color theme"] ();
        label ~at:[cls "toggle-icons"; ("for", "switch")] ~inner:[
          img ~at:[cls "moon"; ("src", "./assets/moon.svg"); ("alt", "Dark mode"); ("loading", "lazy")] ();
          img ~at:[cls "sun"; ("src", "./assets/sun.svg"); ("alt", "Light mode"); ("loading", "lazy")] ()
        ] ()
      ] ()
    ] ()
  ] in

  let nav = el "nav" ~at:[cls "navbar"] ~inner:[
    div ~at:[cls "container"] ~inner:[
      logo;
      ul ~at:[cls "nav-menu"] ~inner:nav_links ();
      button ~at:[cls "hamburger"; ("type", "button"); aria_label "Toggle menu"; ("aria-expanded", "false")]
        ~inner:[span ~at:[cls "bar"] (); span ~at:[cls "bar"] (); span ~at:[cls "bar"] ()] ()
    ] ()
  ] () in

  let header_content = section ~at:[cls "header-container container"] ~inner:[
    p ~at:[cls "eyebrow"] ~inner:[txt "M.Sc. Physics by Research | University of York"] ();
    h1 ~inner:[txt "Alexander Dukhan"] ();
    div ~at:[cls "content-text"] ~inner:[
      h2 ~inner:[txt "Simulating tomorrow's discoveries today."] ();
      p ~inner:[txt "Computational biophysicist modeling matter to uncover mechanism-level insights into living systems, with experience spanning molecular dynamics, statistical analysis, and scientific software tooling."] ()
    ] ();
    div ~at:[cls "hero-cta-row"] ~inner:[
      a ~at:[href "https://www.linkedin.com/in/alexander-dukhan-191574250/"; cls "btn btn-secondary"; ("target", "_blank"); ("rel", "noopener noreferrer")] ~inner:[txt "Connect on LinkedIn"] ();
      a ~at:[href "#"; cls "btn btn-outline"; ("data-email-link", "true")] ~inner:[txt "Email Me"] ()
    ] ();
    ul ~at:[cls "impact-pills"] ~inner:[
      li ~inner:[txt "3 years of active research employment"] ();
      li ~inner:[txt "Wet-lab and computational fluency"] ();
      li ~inner:[txt "HPC and simulation workflow development"] ()
    ] ()
  ] () in

  el "header" ~at:[id_ "hero"] ~inner:[nav; header_content] ()

let render_projects () =
  let projects = [
    project_card ~title:"Prokaryotic Nucleoid Associated Proteins and the ProU70 Promoter Sequence"
      ~desc:"University of York, Noy Lab, Amber, Python, R" ~status:"Research in progress" ~card_cls:"project-card--nucleoid" ();
    project_card ~title:"Tubulin Isotype Variation and Cancer Cell Drug Resistance"
      ~desc:"University of Michigan, Sept Lab, NAMD, VMD, R" ~status:"Academic research" ~card_cls:"project-card--tubulin" ();
    project_card ~title:"Binding Affinities of Membrane Proximal Actin Probes"
      ~desc:"University of Michigan, Veatch Lab, NAMD, AlphaFold, CHARMM-GUI, R" ~status:"Academic research" ~card_cls:"project-card--actin" ();
    project_card ~title:"Simulating Hadron Collisions on the QGP Boundary"
      ~desc:"Personal Project, SMASH, CERN ROOT" ~status:"Independent simulation study" ~card_cls:"project-card--qgp" ();
    project_card ~link:"https://github.com/adukhan99/slurmgen" ~title:"SLURMGEN - A Type-Checked SBATCH Header Generator"
      ~desc:"Personal Project, OCaml, Open Source, HPC Utility" ~status:"" ~card_cls:"project-card--slurmgen" ();
    project_card ~link:"https://github.com/adukhan99/orchcaml" ~title:"OrchCaml - An Agent Orchestration Framework"
      ~desc:"Personal Project, OCaml, Open Source, AI" ~status:"" ~card_cls:"project-card--orchcaml" ();
  ] in

  section ~at:[id_ "projects"; cls "project-container container"] ~inner:[
    div ~at:[cls "division"] ();
    div ~at:[cls "content-text"] ~inner:[
      h2 ~inner:[txt "Selected Research and Engineering Work"] ();
      p ~inner:[txt "Personal and academic projects in molecular biophysics, computational chemistry, and simulation tooling."] ()
    ] ();
    article ~at:[cls "project"] ~inner:projects ()
  ] ()

let render_social () =
  section ~at:[cls "container social-proof"] ~inner:[
    div ~at:[cls "content-text"] ~inner:[h2 ~inner:[txt "Links"] ()] ();
    div ~at:[cls "social-icons"] ~inner:[
      social_link ~url:"https://github.com/adukhan99" ~aria:"GitHub" ~img_src:"assets/github-icon.svg" ();
      social_link ~url:"https://www.linkedin.com/in/alexander-dukhan-191574250/" ~aria:"LinkedIn" ~img_src:"assets/linkedin-icon.svg" ();
      social_link ~url:"#" ~aria:"Email" ~icon_cls:"fas fa-envelope" ()
    ] ()
  ] ()

let render_footer () =
  footer ~at:[id_ "footer"] ~inner:[
    div ~at:[cls "container"] ~inner:[
      a ~at:[("data-email-link", "true"); href "#"] ~inner:[txt "Send me an Email"] ()
    ] ()
  ] ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Logic.Style.inject_main ();
    let body = Document.body G.document in
    El.set_children body [
      render_hero ();
      render_projects ();
      render_social ();
      render_footer ()
    ];
    Logic.setup_logic ();
    Physics.start ();
    Js._true
  )



