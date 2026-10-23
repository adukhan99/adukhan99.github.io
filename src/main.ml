open Brr
open Js_of_ocaml
open Logic.Ui

let email = "#"

let project_card idx (pr : Projects.t) =
  let n = Printf.sprintf "%02d" (idx + 1) in
  let visual = div ~at:[ cls "card-visual" ] () in
  let figure = div ~at:[ cls "card-figure" ] ~inner:[
    visual;
    span ~at:[ cls "corner corner--tl"; hidden ] ();
    span ~at:[ cls "corner corner--br"; hidden ] ();
    p ~at:[ cls "card-caption" ] ~inner:[ txt pr.caption ] ()
  ] () in
  let spec k v = [ dt ~inner:[ txt k ] (); dd ~inner:v () ] in
  let tags = List.map (fun t -> span ~at:[ cls "tag" ] ~inner:[ txt t ] ()) pr.stack in
  let status = match pr.status with
    | Some s -> spec "status" [ span ~at:[ cls "status-dot"; hidden ] (); txt s ]
    | None -> [] in
  let links = List.map (fun (l, u) ->
    a ~at:([ cls "card-link"; href u; aria_label (l ^ " for " ^ pr.slug) ] @ external_)
      ~inner:[ txt ("> open " ^ l ^ " \xe2\x86\x97") ] ()) pr.links in
  let info = div ~at:[ cls "card-info" ] ~inner:([
    h3 ~inner:[ txt pr.title ] ();
    p ~at:[ cls "card-blurb" ] ~inner:[ txt pr.blurb ] ();
    dl ~at:[ cls "spec" ] ~inner:(spec "org" [ txt pr.org ] @ spec "stack" tags @ status) ()
  ] @ links) () in
  let card = article ~at:[ cls "card"; ("data-index", n) ] ~inner:[
    div ~at:[ cls "win-bar" ] ~inner:[
      span ~at:[ cls "win-title" ] ~inner:[ txt (n ^ " \xe2\x94\x80 " ^ pr.slug) ] ();
      span ~at:[ cls "win-ctl"; hidden ] ~inner:[ txt "_ \xe2\x96\xa1 \xc3\x97" ] ()
    ] ();
    div ~at:[ cls "card-body" ] ~inner:[ figure; info ] ()
  ] () in
  (card, fun () -> ignore (Logic.Scene.mount ~host:visual ~track:card pr.visual))

let render_hero () =
  let nav = render_navbar ~links:[ ("#projects", "research"); ("./resume.html", "resume") ] in
  let readout k v = [ dt ~inner:[ txt k ] (); dd ~at:[ ("data-stat", k) ] ~inner:[ txt v ] () ] in
  let main_win = window ~extra:"hero-win" ~title:"adukhan@wired:~" ~sub:"present day, present time" [
    p ~at:[ cls "eyebrow" ] ~inner:[ txt "PhD student in Physics | University of York" ] ();
    h1 ~at:[ cls "sr-only" ] ~inner:[ txt "Alexander Dukhan" ] ();
    pre ~at:[ cls "banner"; hidden ] ~inner:(List.map (fun (shadow, s) ->
      if shadow then span ~at:[ cls "banner-shade" ] ~inner:[ txt s ] () else txt s)
      (Logic.Banner.render [ "Alexander"; "Dukhan" ])) ();
    h2 ~at:[ cls "tagline"; ("data-type", "Simulating tomorrow's discoveries today.") ]
      ~inner:[ txt "Simulating tomorrow's discoveries today." ] ();
    p ~at:[ cls "lede" ] ~inner:[ txt "Computational biophysicist modeling matter to uncover mechanism-level insights into living systems, with experience spanning molecular dynamics, statistical analysis, and scientific software tooling." ] ();
    div ~at:[ cls "cta-row" ] ~inner:[
      a ~at:([ href "https://www.linkedin.com/in/alexander-dukhan-191574250/"; cls "btn btn-solid" ] @ external_) ~inner:[ txt "Connect on LinkedIn" ] ();
      a ~at:[ href email; cls "btn"; ("data-email-link", "true") ] ~inner:[ txt "Email Me" ] ();
      a ~at:[ href "./resume.html"; cls "btn" ] ~inner:[ txt "Resume \xe2\x86\x92" ] ()
    ] ();
    ul ~at:[ cls "readout" ] ~inner:[
      li ~inner:[ txt "3 years of active research employment" ] ();
      li ~inner:[ txt "Wet-lab and computational fluency" ] ();
      li ~inner:[ txt "HPC and simulation workflow development" ] ()
    ] ()
  ] in
  let side = window ~extra:"field-win" ~title:"field.sys" ~sub:"live" [
    dl ~at:[ cls "stats" ] ~inner:(
      readout "model" "gray\xe2\x80\x93scott \xc3\x97 navier\xe2\x80\x93stokes"
      @ readout "regime" "mitosis \xc2\xb7 f=.0367 k=.0649"
      @ readout "grid" "--" @ readout "fps" "--" @ readout "cover" "--" @ readout "taps" "0") ();
    p ~at:[ cls "hint" ] ~inner:[ txt "move to stir \xc2\xb7 tap to perturb" ] ()
  ] in
  let vert = p ~at:[ cls "vert"; ("lang", "ja"); hidden ]
    ~inner:[ txt "\xe3\x83\x97\xe3\x83\xac\xe3\x82\xbc\xe3\x83\xb3\xe3\x83\x88\xe3\x83\xbb\xe3\x83\x87\xe3\x82\xa4\xe3\x80\x80\xe3\x83\x97\xe3\x83\xac\xe3\x82\xbc\xe3\x83\xb3\xe3\x83\x88\xe3\x83\xbb\xe3\x82\xbf\xe3\x82\xa4\xe3\x83\xa0" ] () in
  el "header" ~at:[ id_ "hero" ] ~inner:[
    nav;
    div ~at:[ cls "container hero-grid" ] ~inner:[ main_win; side; vert ] ()
  ] ()

let render_projects () =
  let cards = List.mapi project_card Projects.all in
  let el =
    section ~at:[ id_ "projects"; cls "container" ] ~inner:[
      section_head ~index:"01" ~title:"Selected Research and Engineering Work" ~gloss:"\xe7\xa0\x94\xe7\xa9\xb6"
        ~sub:"Personal and academic projects in molecular biophysics, computational chemistry, and simulation tooling." ();
      div ~at:[ cls "project-list" ] ~inner:(List.map fst cards) ()
    ] ()
  in
  (el, fun () -> List.iter (fun (_, start) -> start ()) cards)

let render_social () =
  section ~at:[ id_ "links"; cls "container" ] ~inner:[
    section_head ~index:"02" ~title:"Links" ~gloss:"\xe6\x8e\xa5\xe7\xb6\x9a" ();
    div ~at:[ cls "badges badges--links" ] ~inner:[
      badge ~at:([ href "https://github.com/adukhan99"; aria_label "GitHub" ] @ external_) ~label:"GITHUB" ~sub:"adukhan99" ~style:"plain" ();
      badge ~at:([ href "https://www.linkedin.com/in/alexander-dukhan-191574250/"; aria_label "LinkedIn" ] @ external_) ~label:"LINKEDIN" ~sub:"connect" ~style:"invert" ();
      badge ~at:[ href "#"; ("data-email-link", "true"); aria_label "Email" ] ~label:"EMAIL" ~sub:"write me" ~style:"accent" ()
    ] ()
  ] ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Logic.Style.inject_main ();
    let body = Document.body G.document in
    let projects, start_cards = render_projects () in
    El.set_children body [
      render_hero ();
      el "main" ~inner:[ projects; render_social () ] ();
      render_footer ()
    ];
    Logic.setup_logic ();
    Logic.Ascii.when_font_ready (fun () ->
      Logic.Field.start ();
      start_cards ());
    Js._true
  )
