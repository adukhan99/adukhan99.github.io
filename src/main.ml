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
    el "li" ~inner:[el "a" ~at:[("class", "nav-link btn btn-primary"); ("href", "./assets/resume.html")]
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

  el "section" ~at:[("id", "projects"); ("class", "project-container container")] ~inner:[
    el "div" ~at:[("class", "division")] ();
    el "div" ~at:[("class", "content-text")] ~inner:[
      el "h2" ~inner:[txt "Selected Research and Engineering Work"] ();
      el "p" ~inner:[txt "Personal and academic projects in molecular biophysics, computational chemistry, and simulation tooling."] ()
    ] ();
    el "article" ~at:[("class", "project")] ~inner:(project_cards @ [slurmgen]) ()
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

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let body = Document.body G.document in
    El.set_children body [
      render_hero ();
      render_projects ();
      render_social ();
      render_footer ()
    ];
    setup_logic ();
    Js._true
  )
