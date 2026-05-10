open Brr
open Js_of_ocaml
open Logic.Ui

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
  let nav = render_navbar ~links:[
    ("./resume.html", "Resume", "btn-primary", Some "fas fa-arrow-right");
    ("#projects", "Research", "btn-outline", None)
  ] in

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
