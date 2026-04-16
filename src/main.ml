open Brr
open Js_of_ocaml

let el name ?(at = []) ?(inner = []) () =
  let e = El.v (Jstr.v name) inner in
  List.iter (fun (k, v) -> El.set_at (Jstr.v k) (Some (Jstr.v v)) e) at;
  e

let txt s = El.txt (Jstr.v s)

let render_hero () =
  let logo_img = el "img" ~at:[("src", "./assets/logo.png"); ("alt", "Alexander Dukhan logo"); ("loading", "lazy")] () in
  let logo_a = el "a" ~at:[("href", "./index.html")] ~inner:[logo_img] () in
  let logo_h1 = el "h1" ~at:[("id", "logo")] ~inner:[logo_a] () in

  let nav_links = [
    el "li" ~inner:[el "a" ~at:[("class", "nav-link btn btn-primary"); ("href", "./resume.html")]
                      ~inner:[txt "Resume "; el "i" ~at:[("class", "fas fa-arrow-right")] ()] ()] ();
    el "li" ~inner:[el "a" ~at:[("class", "nav-link btn btn-outline"); ("href", "#projects")]
                      ~inner:[txt "Research"] ()] ();
    el "li" ~at:[("class", "theme-switch-wrap")] ~inner:[
      el "div" ~at:[("class", "theme-switch")] ~inner:[
        el "input" ~at:[("type", "checkbox"); ("id", "switch"); ("aria-label", "Toggle color theme")] ();
        el "label" ~at:[("class", "toggle-icons"); ("for", "switch")] ~inner:[
          el "img" ~at:[("class", "moon"); ("src", "./assets/moon.svg"); ("alt", "Dark mode"); ("loading", "lazy")] ();
          el "img" ~at:[("class", "sun"); ("src", "./assets/sun.svg"); ("alt", "Light mode"); ("loading", "lazy")] ()
        ] ()
      ] ()
    ] ()
  ] in

  let nav_menu = el "ul" ~at:[("class", "nav-menu")] ~inner:nav_links () in
  let hamburger = el "button" ~at:[("class", "hamburger"); ("type", "button"); ("aria-label", "Toggle menu"); ("aria-expanded", "false")]
                    ~inner:[el "span" ~at:[("class", "bar")] (); el "span" ~at:[("class", "bar")] (); el "span" ~at:[("class", "bar")] ()] () in

  let nav = el "nav" ~at:[("class", "navbar")] ~inner:[
    el "div" ~at:[("class", "container")] ~inner:[logo_h1; nav_menu; hamburger] ()
  ] () in

  let header_content = el "section" ~at:[("class", "header-container container")] ~inner:[
    el "p" ~at:[("class", "eyebrow")] ~inner:[txt "M.Sc. Physics by Research | University of York"] ();
    el "h1" ~inner:[txt "Alexander Dukhan"] ();
    el "div" ~at:[("class", "content-text")] ~inner:[
      el "h2" ~inner:[txt "Simulating tomorrow's discoveries today."] ();
      el "p" ~inner:[txt "Computational biophysicist modeling matter to uncover mechanism-level insights into living systems, with experience spanning molecular dynamics, statistical analysis, and scientific software tooling."] ()
    ] ();
    el "div" ~at:[("class", "hero-cta-row")] ~inner:[
      el "a" ~at:[("href", "https://www.linkedin.com/in/alexander-dukhan-191574250/"); ("class", "btn btn-secondary"); ("target", "_blank"); ("rel", "noopener noreferrer")] ~inner:[txt "Connect on LinkedIn"] ();
      el "a" ~at:[("href", "#"); ("class", "btn btn-outline"); ("data-email-link", "true")] ~inner:[txt "Email Me"] ()
    ] ();
    el "ul" ~at:[("class", "impact-pills")] ~inner:[
      el "li" ~inner:[txt "3 years of active research employment"] ();
      el "li" ~inner:[txt "Wet-lab and computational fluency"] ();
      el "li" ~inner:[txt "HPC and simulation workflow development"] ()
    ] ()
  ] () in

  el "header" ~at:[("id", "hero")] ~inner:[nav; header_content] ()

let render_projects () =
  let projects_data = [
    ("Prokaryotic Nucleoid Associated Proteins and the ProU70 Promoter Sequence", "University of York, Noy Lab, Amber, Python, R", "Research in progress", "project-card--nucleoid");
    ("Tubulin Isotype Variation and Cancer Cell Drug Resistance", "University of Michigan, Sept Lab, NAMD, VMD, R", "Academic research", "project-card--tubulin");
    ("Binding Affinities of Membrane Proximal Actin Probes", "University of Michigan, Veatch Lab, NAMD, AlphaFold, CHARMM-GUI, R", "Academic research", "project-card--actin");
    ("Simulating Hadron Collisions on the QGP Boundary", "Personal Project, SMASH, CERN ROOT", "Independent simulation study", "project-card--qgp");
  ] in

  let project_cards = List.map (fun (title, desc, status, cls) ->
    el "section" ~at:[("class", "card project-card " ^ cls)] ~inner:[
      el "div" ~at:[("class", "project-info")] ~inner:[
        el "div" ~at:[("class", "project-bio")] ~inner:[
          el "h3" ~inner:[txt title] ();
          el "p" ~inner:[txt desc] ();
          el "p" ~at:[("class", "project-status")] ~inner:[txt status] ()
        ] ()
      ] ()
    ] ()
  ) projects_data in

  let slurmgen = el "section" ~at:[("class", "card project-card project-card--slurmgen")] ~inner:[
    el "div" ~at:[("class", "project-info")] ~inner:[
      el "div" ~at:[("class", "project-bio")] ~inner:[
        el "h3" ~inner:[txt "SLURMGEN - A Type-Checked SBATCH Header Generator"] ();
        el "p" ~inner:[txt "Personal Project, OCaml, Open Source, HPC Utility"] ()
      ] ();
      el "div" ~at:[("class", "project-link")] ~inner:[
        el "a" ~at:[("href", "https://github.com/adukhan99/slurmgen"); ("target", "_blank"); ("rel", "noopener noreferrer"); ("aria-label", "Open slurmgen on GitHub")]
          ~inner:[el "i" ~at:[("class", "fas fa-globe")] ()] ()
      ] ()
    ] ()
  ] () in

  let orchcaml = el "section" ~at:[("class", "card project-card project-card--orchcaml")] ~inner:[
    el "div" ~at:[("class", "project-info")] ~inner:[
      el "div" ~at:[("class", "project-bio")] ~inner:[
        el "h3" ~inner:[txt "OrchCaml - An Agent Orchestration Framework"] ();
        el "p" ~inner:[txt "Personal Project, OCaml, Open Source, AI"] ()
      ] ();
      el "div" ~at:[("class", "project-link")] ~inner:[
        el "a" ~at:[("href", "https://github.com/adukhan99/orchcaml"); ("target", "_blank"); ("rel", "noopener noreferrer"); ("aria-label", "Open OrchCaml on GitHub")]
          ~inner:[el "i" ~at:[("class", "fas fa-globe")] ()] ()
      ] ()
    ] ()
  ] () in

  el "section" ~at:[("id", "projects"); ("class", "project-container container")] ~inner:[
    el "div" ~at:[("class", "division")] ();
    el "div" ~at:[("class", "content-text")] ~inner:[
      el "h2" ~inner:[txt "Selected Research and Engineering Work"] ();
      el "p" ~inner:[txt "Personal and academic projects in molecular biophysics, computational chemistry, and simulation tooling."] ()
    ] ();
    el "article" ~at:[("class", "project")] ~inner:(project_cards @ [slurmgen; orchcaml]) ()
  ] ()

let render_social () =
  el "section" ~at:[("class", "container social-proof")] ~inner:[
    el "div" ~at:[("class", "content-text")] ~inner:[el "h2" ~inner:[txt "Links"] ()] ();
    el "div" ~at:[("class", "social-icons")] ~inner:[
      el "a" ~at:[("href", "https://github.com/adukhan99"); ("target", "_blank"); ("rel", "noopener noreferrer"); ("aria-label", "GitHub")]
        ~inner:[el "img" ~at:[("src", "assets/github-icon.svg"); ("alt", "GitHub"); ("loading", "lazy")] ()] ();
      el "a" ~at:[("href", "https://www.linkedin.com/in/alexander-dukhan-191574250/"); ("target", "_blank"); ("rel", "noopener noreferrer"); ("aria-label", "LinkedIn")]
        ~inner:[el "img" ~at:[("src", "assets/linkedin-icon.svg"); ("alt", "LinkedIn"); ("loading", "lazy")] ()] ();
      el "a" ~at:[("href", "#"); ("data-email-link", "true"); ("aria-label", "Email")]
        ~inner:[el "i" ~at:[("class", "fas fa-envelope")] ()] ()
    ] ()
  ] ()

let render_footer () =
  el "footer" ~at:[("id", "footer")] ~inner:[
    el "div" ~at:[("class", "container")] ~inner:[
      el "a" ~at:[("data-email-link", "true"); ("href", "#")] ~inner:[txt "Send me an Email"] ()
    ] ()
  ] ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
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


