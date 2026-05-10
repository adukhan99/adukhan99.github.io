open Brr

module Css = struct
  type prop = string * string
  type rule = string * prop list
  type t =
    | Import of string
    | Rule of string * prop list
    | Media of string * (string * prop list) list
    | Raw of string

  let import s = Import s
  let rule s p = Rule (s, p)
  let media q rs = Media (q, rs)
  let raw s = Raw s

  let render_prop (n, v) = Printf.sprintf "  %s: %s;" n v
  let render_rule (s, ps) =
    Printf.sprintf "%s {\n%s\n}" s (String.concat "\n" (List.map render_prop ps))

  let render_one = function
    | Import s -> Printf.sprintf "@import url(%s);" s
    | Rule (s, ps) -> render_rule (s, ps)
    | Media (q, rs) ->
        let inner =
          List.map (fun (s, ps) ->
            let r = render_rule (s, ps) in
            String.split_on_char '\n' r
            |> List.map (fun line -> "  " ^ line)
            |> String.concat "\n"
          ) rs
          |> String.concat "\n\n"
        in
        Printf.sprintf "@media (%s) {\n%s\n}" q inner
    | Raw s -> s

  let render ts = String.concat "\n\n" (List.map render_one ts)
end

let theme_vars = [
  "--primary-color", "#ffcd42";
  "--secondary-color", "#ffd35c";
  "--accent-color", "#1f7ae0";
  "--bg-primary", "#ffffff";
  "--bg-soft", "#f7f7f2";
  "--text-color", "#191919";
  "--text-color-two", "#ffffff";
  "--bg-secondary", "#111111";
  "--card-background", "#efefea";
  "--bg-secondary-two", "#202020";
  "--border-color", "#e4e4df";
  "--shadow", "0 12px 34px rgba(0, 0, 0, 0.14)";
  "--weight-small", "400";
  "--weight-semibold", "600";
  "--weight-bold", "800";
  "--width-small", "600px";
  "--width-medium", "1100px";
  "--width-large", "1300px";
]

let dark_theme_vars = [
  "--primary-color", "#ffcd42";
  "--secondary-color", "#ffd35c";
  "--accent-color", "#6cb0ff";
  "--bg-primary", "#0d0d0d";
  "--bg-soft", "#151515";
  "--text-color", "#f1f1f1";
  "--text-color-two", "#111111";
  "--bg-secondary", "#f3f3f3";
  "--card-background", "#1a1a1a";
  "--bg-secondary-two", "#ececec";
  "--border-color", "#2f2f2f";
  "--shadow", "0 8px 20px rgba(0, 0, 0, 0.4)";
]

let main_styles = [
  Css.import "https://fonts.googleapis.com/css?family=Raleway:300,400,500,600,700,800";
  Css.rule ":root" theme_vars;
  Css.rule "[data-theme=\"dark\"]" dark_theme_vars;
  Css.rule "html" [
    "font-size", "100%";
    "scroll-behavior", "smooth";
    "background", "var(--bg-primary)";
  ];
  Css.rule "*" [
    "margin", "0";
    "padding", "0";
    "box-sizing", "border-box";
  ];
  Css.rule "ul" ["list-style", "none"];
  Css.rule "a" [
    "text-decoration", "none";
    "color", "var(--text-color)";
  ];
  Css.rule "body" [
    "background", "transparent";
    "color", "var(--text-color)";
    "font-family", "\"Raleway\", sans-serif";
    "line-height", "1.55";
    "position", "relative";
  ];
  Css.rule "#bg-canvas" ["z-index", "0"];
  Css.rule "#hero" ["position", "relative"];
  Css.rule ".header-container" ["position", "relative"; "z-index", "1"];
  Css.rule "#projects, .social-proof, #footer" ["position", "relative"; "z-index", "1"];
  Css.rule ".navbar" [
    "position", "fixed";
    "top", "0";
    "left", "0";
    "right", "0";
    "z-index", "20";
    "background", "var(--bg-primary)";
    "border-bottom", "1px solid transparent";
    "transition", "box-shadow 0.3s ease-in-out, border-color 0.3s ease-in-out";
  ];
  Css.rule ".navbar.scrolled" [
    "box-shadow", "var(--shadow)";
    "border-color", "var(--border-color)";
  ];
  Css.rule ".navbar .container" [
    "display", "flex";
    "align-items", "center";
    "justify-content", "space-between";
    "padding", "0 2rem";
    "min-height", "40px";
    "width", "100%";
  ];
  Css.rule ".navbar #logo img" [
    "display", "block";
    "width", "min(160px, 48vw)";
  ];
  Css.rule ".navbar .nav-menu" [
    "display", "flex";
    "align-items", "center";
    "gap", "0.55rem";
    "background", "transparent";
  ];
  Css.rule ".navbar .nav-link" [
    "font-size", "0.78rem";
    "font-weight", "var(--weight-semibold)";
  ];
  Css.rule ".theme-switch-wrap" ["margin-left", "0.35rem"];
  Css.rule ".fas.fa-arrow-right" [
    "margin-left", "0.45rem";
    "font-size", "0.82rem";
  ];
  Css.rule ".hamburger" [
    "display", "none";
    "border", "0";
    "background", "transparent";
  ];
  Css.rule ".bar" [
    "display", "block";
    "width", "23px";
    "height", "3px";
    "margin", "4px auto";
    "transition", "all 0.3s ease-in-out";
    "border-radius", "30px";
    "background-color", "var(--bg-secondary)";
  ];
  Css.rule "#hero" [
    "display", "flex";
    "flex-direction", "column";
    "padding-top", "160px";
  ];
  Css.rule ".header-container" [
    "display", "flex";
    "flex-direction", "column";
    "align-items", "center";
    "justify-content", "flex-start";
    "margin-top", "0";
    "padding", "1.5rem 1rem 1rem";
  ];
  Css.rule ".eyebrow" [
    "font-size", "0.8rem";
    "letter-spacing", "0.08em";
    "text-transform", "uppercase";
    "color", "color-mix(in srgb, var(--text-color) 66%, transparent)";
  ];
  Css.rule ".header-container > h1" [
    "margin-top", "0.3rem";
    "font-size", "clamp(2rem, 6vw, 3rem)";
  ];
  Css.rule ".content-text" [
    "text-align", "center";
    "margin", "1.4rem 0";
  ];
  Css.rule ".content-text h2" [
    "font-size", "clamp(1.8rem, 5vw, 2.7rem)";
    "line-height", "1.15";
  ];
  Css.rule ".content-text p" [
    "margin", "0 auto";
    "max-width", "730px";
    "padding", "0.55rem";
  ];
  Css.rule ".hero-cta-row" [
    "display", "flex";
    "gap", "0.7rem";
    "flex-wrap", "wrap";
    "justify-content", "center";
  ];
  Css.rule ".impact-pills" [
    "margin-top", "1.4rem";
    "display", "flex";
    "gap", "0.55rem";
    "flex-wrap", "wrap";
    "justify-content", "center";
  ];
  Css.rule ".impact-pills li" [
    "border", "1px solid var(--border-color)";
    "background", "var(--bg-soft)";
    "border-radius", "999px";
    "padding", "0.4rem 0.8rem";
    "font-size", "0.77rem";
  ];
  Css.rule ".division" [
    "width", "100%";
    "height", "1px";
    "background-color", "var(--border-color)";
    "margin", "2rem 0";
  ];
  Css.rule "#projects" [
    "display", "flex";
    "flex-direction", "column";
    "margin", "0 auto 5rem";
  ];
  Css.rule ".project" [
    "display", "grid";
    "grid-template-columns", "repeat(2, minmax(0, 1fr))";
    "grid-auto-rows", "minmax(260px, auto)";
    "gap", "1rem";
  ];
  Css.rule ".project-card" [
    "min-height", "280px";
    "position", "relative";
    "overflow", "hidden";
  ];
  Css.rule ".project-card::before" [
    "content", "\"\"";
    "position", "absolute";
    "inset", "0";
    "background", "linear-gradient(to top, rgba(0, 0, 0, 0.82) 8%, rgba(0, 0, 0, 0.16) 68%)";
  ];
  Css.rule ".project-info" [
    "position", "relative";
    "z-index", "1";
    "display", "flex";
    "height", "100%";
    "justify-content", "space-between";
    "align-items", "flex-end";
    "gap", "0.75rem";
  ];
  Css.rule ".project-bio" [
    "display", "flex";
    "flex-direction", "column";
    "gap", "0.2rem";
  ];
  Css.rule ".project-bio h3" ["font-size", "1rem"];
  Css.rule ".project-bio p" ["font-size", "0.83rem"];
  Css.rule ".project-status" [
    "display", "inline-flex";
    "align-items", "center";
    "width", "fit-content";
    "margin-top", "0.35rem";
    "border-radius", "999px";
    "padding", "0.18rem 0.55rem";
    "background", "color-mix(in srgb, var(--primary-color) 78%, #ffffff 22%)";
    "color", "#111";
    "font-weight", "var(--weight-semibold)";
    "font-size", "0.72rem";
  ];
  Css.rule ".project-link" ["margin-bottom", "0.2rem"];
  Css.rule ".card.project-card--nucleoid" ["background", "url(./assets/project/NAP.png) center center/cover"];
  Css.rule ".card.project-card--tubulin" ["background", "url(./assets/project/tubulin.gif) center center/cover"];
  Css.rule ".card.project-card--actin" ["background", "url(./assets/project/MPActs.gif) center center/cover"];
  Css.rule ".card.project-card--qgp" ["background", "url(./assets/project/QGP.gif) center center/cover"];
  Css.rule ".card.project-card--slurmgen" ["background", "url(./assets/project/slurmgen.jpg) center center/cover"];
  Css.rule ".social-proof" ["margin-bottom", "4rem"];
  Css.rule ".social-icons" [
    "display", "flex";
    "justify-content", "center";
    "gap", "0.9rem";
    "flex-wrap", "wrap";
  ];
  Css.rule ".social-icons img" [
    "width", "26px";
    "height", "26px";
    "opacity", "0.9";
  ];
  Css.rule ".social-icons i" [
    "font-size", "1.1rem";
    "color", "var(--text-color)";
  ];
  Css.rule ".social-icons a" [
    "display", "inline-flex";
    "border", "1px solid var(--border-color)";
    "background", "var(--bg-soft)";
    "border-radius", "14px";
    "padding", "0.6rem";
    "transition", "transform 0.2s ease, border-color 0.2s ease";
  ];
  Css.rule ".social-icons a:hover" [
    "transform", "translateY(-3px)";
    "border-color", "color-mix(in srgb, var(--primary-color) 70%, var(--border-color))";
  ];
  Css.rule "#footer" ["background", "#111111"];
  Css.rule "#footer .container" [
    "display", "flex";
    "align-items", "center";
    "justify-content", "center";
    "color", "#ffffff";
    "min-height", "140px";
    "text-align", "center";
  ];
  Css.rule "#footer a" ["font-size", "0.8rem"; "color", "#fff"];
  Css.rule "#footer a:hover" ["opacity", "0.7"];
  Css.media "max-width: 1000px" [
    ".project", ["grid-template-columns", "1fr"];
  ];
  Css.media "max-width: 670px" [
    ".navbar .container", ["padding", "0 1rem"];
    ".navbar .nav-menu", [
      "position", "fixed";
      "right", "-100vw";
      "top", "4.6rem";
      "flex-direction", "column";
      "width", "min(340px, calc(100% - 30px))";
      "transition", "right 0.3s ease";
      "box-shadow", "var(--shadow)";
      "padding", "1rem";
      "border-radius", "9px";
      "align-items", "stretch";
      "background", "var(--bg-primary)";
      "border", "1px solid var(--border-color)";
    ];
    ".nav-menu.active", ["right", "15px"];
    ".nav-menu li", ["width", "100%"];
    ".nav-menu .btn", ["width", "100%"; "text-align", "center"];
    ".theme-switch-wrap", ["display", "flex"; "justify-content", "center"; "margin-top", "0.3rem"];
    ".hamburger", ["display", "block"; "cursor", "pointer"; "padding", "0.2rem 0.4rem"];
    ".hamburger.active .bar:nth-child(2)", ["opacity", "0"];
    ".hamburger.active .bar:nth-child(1)", ["transform", "translateY(7px) rotate(45deg)"];
    ".hamburger.active .bar:nth-child(3)", ["transform", "translateY(-7px) rotate(-45deg)"];
  ];
  Css.media "max-width: 600px" [
    ".hero-cta-row, .hero-cta-row .btn", ["width", "100%"];
    ".hero-cta-row .btn", ["border-radius", "8px"];
    ".impact-pills", ["justify-content", "flex-start"];
    ".impact-pills li", ["width", "100%"];
  ];
]

let utility_styles = [
  Css.rule ".container" [
    "max-width", "var(--width-medium)";
    "margin", "0 auto";
    "padding", "1rem 2rem";
  ];
  Css.rule "h1::selection, h2::selection" [
    "color", "#111";
    "background", "var(--primary-color)";
  ];
  Css.rule ".btn" [
    "display", "inline-block";
    "padding", "0.75rem 1.15rem";
    "border-radius", "999px";
    "text-transform", "uppercase";
    "letter-spacing", "0.03em";
    "font-size", "0.78rem";
    "font-weight", "var(--weight-semibold)";
    "transition", "0.25s ease";
    "border", "1px solid transparent";
  ];
  Css.rule ".btn-primary" [
    "background", "var(--primary-color)";
    "color", "#000000";
  ];
  Css.rule ".btn-primary:hover" ["background", "var(--secondary-color)"];
  Css.rule ".btn-secondary" [
    "background-color", "var(--bg-secondary)";
    "color", "var(--bg-primary)";
  ];
  Css.rule ".btn-secondary:hover" ["opacity", "0.9"];
  Css.rule ".btn-outline" [
    "border-color", "var(--border-color)";
    "background", "transparent";
    "color", "var(--text-color)";
  ];
  Css.rule ".btn-outline:hover" [
    "border-color", "color-mix(in srgb, var(--primary-color) 75%, var(--border-color))";
    "background", "var(--bg-soft)";
  ];
  Css.rule "#switch" ["display", "none"];
  Css.rule ".toggle-icons" [
    "display", "flex";
    "justify-content", "space-between";
    "align-items", "center";
    "cursor", "pointer";
    "gap", "0.15rem";
  ];
  Css.rule ".toggle-icons > img" [
    "transition", "0.5s cubic-bezier(0.23, 1, 0.32, 1)";
    "width", "28px";
  ];
  Css.rule ".moon" ["transform", "rotate(10deg)"];
  Css.rule "#switch:checked + .toggle-icons .moon" ["transform", "rotate(250deg)"];
  Css.rule "#switch:checked + .toggle-icons .sun" ["transform", "rotate(100deg)"];
  Css.rule ".card" [
    "padding", "1rem";
    "background", "transparent";
    "color", "#ffffff";
    "border-radius", "10px";
    "transition", "0.3s ease-in-out";
  ];
  Css.rule ".card a" [
    "color", "#ffffff";
    "transition", "0.2s ease-in-out";
    "font-size", "1.1rem";
  ];
  Css.rule ".card a:hover" ["color", "var(--primary-color)"];
  Css.rule ".card:hover" [
    "transform", "translateY(-3px)";
    "box-shadow", "var(--shadow)";
  ];
  Css.media "max-width: 768px" [
    ".container", ["padding", "0.9rem 1rem"];
  ];
]

let resume_styles = [
  Css.rule "#resume-content" ["margin-top", "1rem"];
  Css.rule "#resume-content .section" ["margin-bottom", "2rem"];
  Css.rule "#resume-content .content-text" ["text-align", "left"; "margin-bottom", "2rem"];
  Css.rule "#resume-content .content-text p" [
    "margin", "5px 0";
    "font-size", "1.1em";
    "color", "var(--text-color)";
  ];
  Css.rule "#resume-content a" ["color", "var(--primary-color)"; "text-decoration", "none"];
  Css.rule "#resume-content a:hover" ["text-decoration", "underline"];
  Css.rule ".header-container" ["margin-top", "4rem"];
  Css.rule ".header-container h1" ["font-size", "2.2rem"];
  Css.rule ".header-container h2" ["font-size", "1.6rem"; "margin-top", "0.5rem"];
  Css.rule ".section" ["margin-bottom", "0.5rem"];
  Css.rule ".section h2" [
    "padding-bottom", "0.5rem";
    "margin-bottom", "1rem";
    "font-size", "1.8em";
    "color", "var(--text-color)";
    "text-align", "left";
  ];
  Css.rule ".job, .education-item" ["margin-bottom", "1rem"];
  Css.rule ".job-title, .degree" [
    "font-weight", "var(--weight-semibold)";
    "font-size", "1.2em";
    "color", "var(--text-color)";
  ];
  Css.rule ".company, .school" [
    "font-style", "italic";
    "color", "var(--text-color)";
    "opacity", "0.8";
  ];
  Css.rule ".date" ["float", "right"; "color", "var(--text-color)"; "opacity", "0.7"];
  Css.rule "ul.highlights" ["list-style-type", "disc"; "padding-left", "20px"; "color", "var(--text-color)"];
  Css.rule ".skills-section ul" ["list-style-type", "none"; "padding", "0"];
  Css.rule ".skills-section li" ["padding", "8px 0"; "color", "var(--text-color)"];
  Css.rule ".skills-category" ["font-weight", "var(--weight-semibold)"; "color", "var(--primary-color)"];
  Css.media "max-width: 768px" [
    "#resume-content", ["margin-top", "1rem"];
    ".section h2", ["font-size", "1.5em"];
    ".date", ["float", "none"; "display", "block"; "margin-bottom", "5px"];
    ".header-container h1", ["font-size", "1.8rem"];
    ".header-container h2", ["font-size", "1.4rem"];
  ];
]

let inject ts =
  let css_text = Css.render ts in
  let style = El.v (Jstr.v "style") [] in
  El.set_prop (El.Prop.jstr (Jstr.v "innerText")) (Jstr.v css_text) style;
  let head = Document.head G.document in
  El.set_children head (style :: (El.children head))

let inject_main () =
  inject (main_styles @ utility_styles)

let inject_resume () =
  inject (main_styles @ utility_styles @ resume_styles)
