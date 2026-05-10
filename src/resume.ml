open Brr
open Js_of_ocaml
open Logic.Ui

let render_header () =
  let nav = render_navbar ~links:[
    ("./index.html", "HOME", "btn-primary", Some "fas fa-arrow-left")
  ] in
  let header_content = section ~at:[cls "header-container"] ~inner:[
    h1 ~inner:[txt "Alexander Dukhan"] ();
    div ~at:[cls "content-text"] ~inner:[h2 ~inner:[txt "Resume"] ()] ()
  ] () in
  el "header" ~at:[id_ "hero"] ~inner:[nav; header_content] ()

let render_summary () =
  section ~at:[cls "section"] ~inner:[
    h2 ~inner:[txt "Summary"] ();
    p ~inner:[txt "Computational biophysicist with expertise in both wet lab and computational techniques and a passion for mechanistic investigation."] ()
  ] ()

let education_item ~degree ~date ~school ~desc () =
  div ~at:[cls "education-item"] ~inner:[
    p ~inner:[span ~at:[cls "degree"] ~inner:[txt degree] (); span ~at:[cls "date"] ~inner:[txt date] ()] ();
    p ~at:[cls "school"] ~inner:[txt school] ();
    p ~inner:[txt desc] ()
  ] ()

let render_education () =
  section ~at:[cls "section"] ~inner:[
    h2 ~inner:[txt "Education"] ();
    education_item ~degree:"Postgraduate" ~date:"9/2025 - Present" ~school:"University of York, York, UK"
      ~desc:"M.Sc. in Physics by Research under Dr. Agnes Noy and Dr. Mark Leake." ();
    education_item ~degree:"Undergraduate" ~date:"8/2022 - 5/2025" ~school:"University of Michigan, Ann Arbor, MI"
      ~desc:"B.Sc. in Biochemistry, completed in three years alongside two years of active research employment." ();
    education_item ~degree:"Secondary" ~date:"9/2018 - 5/2022" ~school:"Royal Oak High School, Royal Oak, MI"
      ~desc:"Graduated with high honors, 4.2 GPA, and 30 credits completed towards a Bachelor's degree." ()
  ] ()

let job_item ~title ~date ~company ~highlights () =
  div ~at:[cls "job"] ~inner:[
    p ~inner:[span ~at:[cls "job-title"] ~inner:[txt title] (); span ~at:[cls "date"] ~inner:[txt date] ()] ();
    p ~at:[cls "company"] ~inner:[txt company] ();
    ul ~inner:(List.map (fun h -> li ~inner:[txt h] ()) highlights) ()
  ] ()

let render_employment () =
  section ~at:[cls "section"] ~inner:[
    h2 ~inner:[txt "Employment"] ();
    job_item ~title:"Researcher" ~date:"8/2023 - 8/2025" ~company:"University of Michigan, Ann Arbor, MI"
      ~highlights:["Two years of research experience across both the Sept and Veatch labs conducting molecular dynamics."] ();
    job_item ~title:"Development Director" ~date:"10/2022 - Present" ~company:"Great Lakes Invitational Conference Association"
      ~highlights:["Development director and website manager assisting this 501(c)(3) with grant writing and outreach as well as the technology sphere."] ()
  ] ()

let render_skills () =
  let skill_cat ~name ~items =
    li ~inner:[span ~at:[cls "skills-category"] ~inner:[txt (name ^ ": ")] (); txt (String.concat ", " items)] ()
  in
  section ~at:[cls "section skills-section"] ~inner:[
    h2 ~inner:[txt "Technical Skills"] ();
    ul ~inner:[
      skill_cat ~name:"Data Analysis and Visualization" ~items:["R"; "Python"; "Fortran"; "Rust"; "OCaml"; "C/C++"; "Chapel"];
      skill_cat ~name:"Molecular Dynamics" ~items:["NAMD/VMD"; "Amber/AmberTools"; "GROMACS"];
      skill_cat ~name:"Wet-lab Biochemistry" ~items:["Cell culture"; "flow cytometry"; "STORM"; "FRET"];
      skill_cat ~name:"Scientific Communication" ~items:["LaTeX"; "Overleaf"; "Microsoft/Google Office Software"]
    ] ()
  ] ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Logic.Style.inject_resume ();
    let body = Document.body G.document in
    El.set_children body [
      render_header ();
      section ~at:[id_ "resume-content"; cls "project-container container"] ~inner:[
        div ~at:[cls "content-text"] ~inner:[
          p ~inner:[txt "York, UK | +1 (248) 404-0862 | "; a ~at:[("data-email-link", "true"); href "#"] ~inner:[txt "agdukhan AT gmail DOT com"] ()] ()
        ] ();
        render_summary ();
        render_education ();
        render_employment ();
        render_skills ()
      ] ();
      render_footer ()
    ];
    Logic.setup_logic ();
    Js._true
  )
