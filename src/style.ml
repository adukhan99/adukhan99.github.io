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


(* ---- Tokens ----------------------------------------------------------
   Phosphor (dark) is the default; paper (light) is opt-in. The --field-*
   and --glyph-* bands are read by Ascii.Palette for the canvas surfaces. *)

let phosphor = [
  "--bg", "#050607";
  "--panel", "rgba(8, 10, 12, 0.88)";
  "--bar", "#10151a";
  "--line", "#26323a";
  "--grid", "rgba(127, 240, 230, 0.05)";
  "--text", "#d9e2df";
  "--muted", "#80908c";
  "--accent", "#ff3355";
  "--cyan", "#7ff0e6";
  "--on-accent", "#ffffff";
  "--shadow", "rgba(0, 0, 0, 0.55)";
  "--bevel-hi", "#5b6b72";
  "--bevel-lo", "#000000";
  "--scan", "rgba(0, 0, 0, 0.16)";
  "--field-0", "rgba(110, 190, 200, 0.14)";
  "--field-1", "rgba(127, 220, 220, 0.27)";
  "--field-2", "rgba(160, 240, 236, 0.46)";
  "--field-3", "rgba(255, 51, 85, 0.78)";
  "--glyph-0", "rgba(127, 240, 230, 0.30)";
  "--glyph-1", "rgba(127, 240, 230, 0.62)";
  "--glyph-2", "#d2fff9";
  "--glyph-3", "#ff3355";
]

let paper = [
  "--bg", "#e8e5da";
  "--panel", "rgba(243, 241, 233, 0.92)";
  "--bar", "#d6d1c1";
  "--line", "#aaa392";
  "--grid", "rgba(0, 70, 80, 0.06)";
  "--text", "#141618";
  "--muted", "#5a5e5b";
  "--accent", "#c8002a";
  "--cyan", "#00626a";
  "--on-accent", "#ffffff";
  "--shadow", "rgba(40, 34, 20, 0.28)";
  "--bevel-hi", "#ffffff";
  "--bevel-lo", "#77705f";
  "--scan", "rgba(60, 50, 30, 0.05)";
  "--field-0", "rgba(20, 60, 70, 0.13)";
  "--field-1", "rgba(20, 70, 80, 0.24)";
  "--field-2", "rgba(10, 60, 70, 0.42)";
  "--field-3", "rgba(200, 0, 42, 0.62)";
  "--glyph-0", "rgba(0, 80, 90, 0.34)";
  "--glyph-1", "rgba(0, 80, 90, 0.66)";
  "--glyph-2", "#0b2a2e";
  "--glyph-3", "#c8002a";
]

let fonts = [
  "--mono", "\"IBM Plex Mono\", ui-monospace, Menlo, Consolas, monospace";
  "--display", "\"VT323\", \"IBM Plex Mono\", monospace";
  "--jp", "\"DotGothic16\", \"IBM Plex Mono\", sans-serif";
  "--wide", "1120px";
  "--hard", "6px 6px 0 var(--shadow)";
]

let base = [
  Css.rule ":root" (fonts @ phosphor);
  Css.rule ":root[data-theme=\"light\"]" paper;
  Css.rule "*, *::before, *::after" [ "margin", "0"; "padding", "0"; "box-sizing", "border-box" ];
  Css.rule "html" [ "scroll-behavior", "smooth"; "background", "var(--bg)"; "color-scheme", "dark" ];
  Css.rule ":root[data-theme=\"light\"]" [ "color-scheme", "light" ];
  Css.rule "body" [
    "background", "var(--bg)";
    "color", "var(--text)";
    "font-family", "var(--mono)";
    "font-size", "15px";
    "line-height", "1.6";
    "min-height", "100vh";
    "overflow-x", "hidden";
  ];
  Css.rule "ul" [ "list-style", "none" ];
  Css.rule "a" [ "color", "inherit"; "text-decoration", "none" ];
  Css.rule "::selection" [ "background", "var(--accent)"; "color", "var(--on-accent)" ];
  Css.rule ":focus-visible" [ "outline", "2px dashed var(--accent)"; "outline-offset", "3px" ];
  Css.rule ".sr-only" [
    "position", "absolute"; "width", "1px"; "height", "1px"; "overflow", "hidden";
    "clip", "rect(0 0 0 0)"; "white-space", "nowrap";
  ];
  Css.rule ".container" [ "max-width", "var(--wide)"; "margin", "0 auto"; "padding", "0 16px" ];
  (* The simulation sits under everything; content floats on top. *)
  Css.rule "#field" [
    "position", "fixed"; "inset", "0"; "width", "100%"; "height", "100%";
    "z-index", "0"; "pointer-events", "none";
  ];
  Css.rule "#hero, main, #footer, #resume-content" [ "position", "relative"; "z-index", "1" ];
  (* CRT: scanlines plus a soft vignette, above content but inert. *)
  Css.rule "body::after" [
    "content", "\"\"";
    "position", "fixed"; "inset", "0"; "z-index", "40"; "pointer-events", "none";
    "background",
      "repeating-linear-gradient(to bottom, transparent 0 2px, var(--scan) 2px 3px), \
       radial-gradient(ellipse at center, transparent 55%, var(--scan) 100%)";
  ];
]

(* ---- Chrome: menubar, windows, buttons ------------------------------- *)

let chrome = [
  Css.rule ".navbar" [
    "position", "fixed"; "top", "0"; "left", "0"; "right", "0"; "z-index", "30";
    "height", "40px";
    "background", "color-mix(in srgb, var(--bar) 92%, transparent)";
    "backdrop-filter", "blur(6px)";
    "border-bottom", "1px solid var(--line)";
    "font-size", "12px";
    "text-transform", "uppercase";
    "letter-spacing", "0.12em";
    "transition", "box-shadow 0.3s";
  ];
  Css.rule ".navbar.scrolled" [ "box-shadow", "0 4px 0 var(--shadow)" ];
  Css.rule ".navbar-inner" [
    "max-width", "var(--wide)"; "height", "100%"; "margin", "0 auto"; "padding", "0 16px";
    "display", "flex"; "align-items", "center"; "gap", "18px";
  ];
  Css.rule ".brand" [ "display", "flex"; "align-items", "center"; "gap", "8px"; "font-weight", "600" ];
  Css.rule ".brand img" [ "width", "24px"; "height", "24px"; "display", "block" ];
  Css.rule ".brand-sub" [ "color", "var(--accent)"; "font-weight", "400" ];
  Css.rule ".menu" [ "display", "flex"; "gap", "2px"; "margin-left", "auto" ];
  Css.rule ".menu-link" [ "display", "block"; "padding", "3px 8px"; "white-space", "nowrap" ];
  Css.rule ".menu-link::before" [ "content", "\"[\""; "color", "var(--muted)"; "margin-right", "4px" ];
  Css.rule ".menu-link::after" [ "content", "\"]\""; "color", "var(--muted)"; "margin-left", "4px" ];
  Css.rule ".menu-link:hover" [ "background", "var(--text)"; "color", "var(--bg)" ];
  Css.rule ".navbar-right" [ "display", "flex"; "align-items", "center"; "gap", "14px" ];
  Css.rule "#switch" [ "position", "absolute"; "opacity", "0"; "width", "1px"; "height", "1px" ];
  Css.rule ".theme-toggle" [
    "display", "block"; "cursor", "pointer"; "padding", "2px 8px";
    "border", "1px solid var(--line)"; "white-space", "nowrap"; "user-select", "none";
  ];
  Css.rule ".theme-toggle:hover" [ "border-color", "var(--accent)" ];
  Css.rule "#switch:focus-visible + .theme-toggle" [ "outline", "2px dashed var(--accent)"; "outline-offset", "2px" ];
  Css.rule ".t-light" [ "display", "none" ];
  Css.rule "[data-theme=\"light\"] .t-light" [ "display", "inline" ];
  Css.rule "[data-theme=\"light\"] .t-dark" [ "display", "none" ];
  Css.rule ".clock" [ "color", "var(--muted)"; "font-variant-numeric", "tabular-nums"; "white-space", "nowrap" ];
  Css.rule ".clock-zone" [ "color", "var(--accent)"; "margin-right", "6px" ];

  Css.rule ".win" [
    "background", "var(--panel)";
    "border", "1px solid var(--line)";
    "box-shadow", "var(--hard)";
    "backdrop-filter", "blur(2px)";
  ];
  Css.rule ".win-bar" [
    "display", "flex"; "align-items", "center"; "gap", "10px";
    "padding", "4px 10px";
    "background", "var(--bar)";
    "border-bottom", "1px solid var(--line)";
    "font-size", "11px"; "text-transform", "uppercase"; "letter-spacing", "0.14em";
    "white-space", "nowrap";
  ];
  Css.rule ".win-title" [ "font-weight", "600" ];
  Css.rule ".win-title::before" [ "content", "\"\\25A0  \""; "color", "var(--accent)" ];
  Css.rule ".win-sub" [ "flex", "1"; "min-width", "0"; "overflow", "hidden"; "text-overflow", "ellipsis"; "color", "var(--muted)" ];
  Css.rule ".win-ctl" [ "margin-left", "auto"; "color", "var(--muted)"; "letter-spacing", "0.4em" ];
  Css.rule ".win-body" [ "padding", "24px" ];

  Css.rule ".btn" [
    "display", "inline-block";
    "padding", "8px 14px";
    "border", "1px solid var(--text)";
    "background", "var(--panel)";
    "color", "var(--text)";
    "font-size", "12px"; "text-transform", "uppercase"; "letter-spacing", "0.1em";
    "box-shadow", "3px 3px 0 var(--accent)";
    "transition", "transform 0.12s, box-shadow 0.12s";
  ];
  Css.rule ".btn:hover" [ "transform", "translate(-2px, -2px)"; "box-shadow", "5px 5px 0 var(--accent)" ];
  Css.rule ".btn:active" [ "transform", "translate(3px, 3px)"; "box-shadow", "0 0 0 var(--accent)" ];
  Css.rule ".btn-solid" [ "background", "var(--text)"; "color", "var(--bg)" ];

  Css.rule ".badges" [ "display", "flex"; "flex-wrap", "wrap"; "gap", "8px"; "justify-content", "center" ];
  Css.rule ".badge" [
    "width", "88px"; "height", "31px";
    "display", "inline-flex"; "flex-direction", "column"; "align-items", "center"; "justify-content", "center";
    "border", "2px solid"; "border-color", "var(--bevel-hi) var(--bevel-lo) var(--bevel-lo) var(--bevel-hi)";
    "line-height", "1"; "text-align", "center"; "overflow", "hidden";
    "background", "var(--panel)"; "color", "var(--text)";
  ];
  Css.rule ".badge-main" [ "font-family", "var(--display)"; "font-size", "17px"; "letter-spacing", "0.06em" ];
  Css.rule ".badge-sub" [ "font-size", "8px"; "text-transform", "uppercase"; "letter-spacing", "0.18em"; "opacity", "0.8" ];
  Css.rule ".badge--invert" [ "background", "var(--text)"; "color", "var(--bg)" ];
  Css.rule ".badge--accent" [ "background", "var(--accent)"; "color", "var(--on-accent)" ];
  Css.rule ".badge--ocaml" [ "background", "#ec6813"; "color", "#111" ];
  Css.rule "a.badge" [ "transition", "transform 0.12s, box-shadow 0.12s" ];
  Css.rule "a.badge:hover" [ "transform", "translate(-2px, -2px)"; "box-shadow", "4px 4px 0 var(--shadow)" ];
  Css.rule ".badges--links" [ "justify-content", "flex-start"; "gap", "14px" ];
  Css.rule ".badges--links .badge" [ "width", "132px"; "height", "46px" ];
  Css.rule ".badges--links .badge-main" [ "font-size", "24px" ];
  Css.rule ".badges--links .badge-sub" [ "font-size", "9px" ];
]

(* ---- Home page ------------------------------------------------------- *)

let home = [
  Css.rule "#hero" [ "padding-top", "72px" ];
  Css.rule ".hero-grid" [
    "position", "relative";
    "display", "grid"; "grid-template-columns", "minmax(0, 1fr) 280px"; "gap", "20px"; "align-items", "start";
  ];
  Css.rule ".hero-win .win-body" [ "container-type", "inline-size" ];
  Css.rule ".eyebrow" [ "font-size", "12px"; "text-transform", "uppercase"; "letter-spacing", "0.16em"; "color", "var(--muted)" ];
  Css.rule ".banner" [
    "font-family", "var(--mono)";
    "font-size", "12px";
    "font-size", "min(19px, calc(100cqi / 32.5))";
    "line-height", "1";
    "margin", "18px 0 22px";
    "white-space", "pre"; "overflow", "hidden";
    "color", "var(--text)";
    "animation", "glitch 7s steps(1) infinite";
  ];
  Css.raw "@keyframes glitch {\n  0%, 91%, 100% { transform: none; text-shadow: none; }\n  92% { transform: translateX(3px); text-shadow: -3px 0 var(--accent), 3px 0 var(--cyan); }\n  93% { transform: translateX(-2px) skewX(-8deg); text-shadow: 2px 0 var(--accent); }\n  94% { transform: none; text-shadow: -1px 0 var(--cyan); }\n}";
  Css.rule ".banner-shade" [ "color", "var(--accent)"; "opacity", "0.75" ];
  Css.rule ".tagline" [
    "font-family", "var(--display)"; "font-weight", "400";
    "font-size", "clamp(28px, 4.4vw, 44px)"; "line-height", "1.05";
    "min-height", "1.05em";
    "margin-bottom", "14px";
  ];
  Css.rule ".typing::after" [
    "content", "\"_\""; "color", "var(--accent)"; "margin-left", "4px";
    "animation", "blink 1s steps(1) infinite";
  ];
  Css.raw "@keyframes blink { 50% { opacity: 0; } }";
  Css.rule ".lede" [ "max-width", "62ch"; "color", "var(--text)"; "opacity", "0.86" ];
  Css.rule ".cta-row" [ "display", "flex"; "flex-wrap", "wrap"; "gap", "12px"; "margin", "24px 0 22px" ];
  Css.rule ".readout" [ "border-top", "1px dashed var(--line)"; "padding-top", "14px"; "font-size", "13px"; "color", "var(--muted)" ];
  Css.rule ".readout li::before" [ "content", "\"> \""; "color", "var(--accent)" ];
  Css.rule ".field-win" [ "position", "sticky"; "top", "60px"; "font-size", "12px" ];
  Css.rule ".field-win .win-body" [ "padding", "16px" ];
  Css.rule ".stats" [ "display", "grid"; "grid-template-columns", "auto 1fr"; "gap", "6px 14px" ];
  Css.rule ".stats dt" [ "color", "var(--muted)"; "text-transform", "uppercase"; "letter-spacing", "0.12em"; "font-size", "10px"; "padding-top", "2px" ];
  Css.rule ".stats dd" [ "font-variant-numeric", "tabular-nums"; "overflow-wrap", "anywhere" ];
  Css.rule ".hint" [ "margin-top", "16px"; "color", "var(--accent)"; "text-transform", "uppercase"; "letter-spacing", "0.14em"; "font-size", "10px" ];
  Css.rule ".hint::before" [ "content", "\"\\25B8  \"" ];
  Css.rule ".vert" [
    "position", "absolute"; "top", "8px"; "right", "-34px";
    "writing-mode", "vertical-rl";
    "font-family", "var(--jp)"; "font-size", "13px"; "letter-spacing", "0.35em";
    "color", "var(--accent)"; "opacity", "0.8";
  ];

  Css.rule ".section-head" [
    "margin", "96px 0 28px";
    "padding", "16px 20px";
    "background", "var(--panel)";
    "border", "1px solid var(--line)";
    "border-left", "4px solid var(--accent)";
    "box-shadow", "var(--hard)";
  ];
  Css.rule ".section-index" [ "color", "var(--accent)"; "font-size", "12px"; "letter-spacing", "0.2em" ];
  Css.rule ".section-head h2" [
    "font-family", "var(--display)"; "font-weight", "400";
    "font-size", "clamp(32px, 5vw, 52px)"; "line-height", "1";
    "display", "flex"; "flex-wrap", "wrap"; "align-items", "baseline"; "gap", "6px 16px";
  ];
  Css.rule ".gloss" [ "font-family", "var(--jp)"; "font-size", "0.42em"; "color", "var(--accent)"; "letter-spacing", "0.3em" ];
  Css.rule ".section-sub" [ "margin-top", "10px"; "max-width", "70ch"; "color", "var(--muted)"; "font-size", "14px" ];

  Css.rule ".project-list" [ "display", "grid"; "gap", "40px" ];
  Css.rule ".card" [
    "position", "relative";
    "background", "var(--panel)";
    "border", "1px solid var(--line)";
    "clip-path", "polygon(0 0, calc(100% - 22px) 0, 100% 22px, 100% 100%, 22px 100%, 0 calc(100% - 22px))";
    "transition", "border-color 0.25s";
  ];
  (* Draw the bevelled corners the clip-path cut away from the border. *)
  Css.rule ".card::before, .card::after" [
    "content", "\"\""; "position", "absolute"; "width", "22px"; "height", "22px"; "z-index", "2";
    "pointer-events", "none";
  ];
  Css.rule ".card::before" [
    "top", "0"; "right", "0";
    "background", "linear-gradient(to top right, transparent calc(50% - 1px), var(--accent) calc(50% - 1px), var(--accent) calc(50% + 1px), transparent calc(50% + 1px))";
  ];
  Css.rule ".card::after" [
    "bottom", "0"; "left", "0";
    "background", "linear-gradient(to top right, transparent calc(50% - 1px), var(--line) calc(50% - 1px), var(--line) calc(50% + 0.5px), transparent calc(50% + 0.5px))";
  ];
  Css.rule ".card:hover" [ "border-color", "color-mix(in srgb, var(--accent) 55%, var(--line))" ];
  Css.rule ".card .win-bar" [ "padding-right", "30px" ];
  Css.rule ".card-body" [ "display", "grid"; "grid-template-columns", "minmax(0, 1.35fr) minmax(0, 1fr)" ];
  Css.rule ".card-figure" [
    "position", "relative"; "min-height", "340px";
    "border-right", "1px solid var(--line)";
    "background-color", "color-mix(in srgb, var(--bg) 70%, transparent)";
    "background-image",
      "radial-gradient(ellipse at center, color-mix(in srgb, var(--cyan) 9%, transparent), transparent 70%), \
       linear-gradient(var(--grid) 1px, transparent 1px), linear-gradient(90deg, var(--grid) 1px, transparent 1px)";
    "background-size", "100% 100%, 22px 22px, 22px 22px";
    "cursor", "crosshair";
  ];
  Css.rule ".card:nth-child(even) .card-figure" [ "order", "2"; "border-right", "0"; "border-left", "1px solid var(--line)" ];
  Css.rule ".card-visual" [ "position", "absolute"; "inset", "0 0 30px 0" ];
  Css.rule ".card-visual canvas" [ "display", "block"; "width", "100%"; "height", "100%" ];
  Css.rule ".corner" [ "position", "absolute"; "width", "14px"; "height", "14px"; "border", "0 solid var(--accent)" ];
  Css.rule ".corner--tl" [ "top", "10px"; "left", "10px"; "border-top-width", "2px"; "border-left-width", "2px" ];
  Css.rule ".corner--br" [ "bottom", "40px"; "right", "10px"; "border-bottom-width", "2px"; "border-right-width", "2px" ];
  Css.rule ".card-caption" [
    "position", "absolute"; "left", "0"; "right", "0"; "bottom", "0";
    "height", "30px"; "padding", "6px 12px";
    "border-top", "1px dashed var(--line)";
    "font-size", "11px"; "color", "var(--muted)";
    "white-space", "nowrap"; "overflow", "hidden"; "text-overflow", "ellipsis";
  ];
  Css.rule ".card-info" [ "padding", "24px"; "display", "flex"; "flex-direction", "column"; "gap", "14px" ];
  Css.rule ".card-info h3" [ "font-family", "var(--display)"; "font-weight", "400"; "font-size", "30px"; "line-height", "1.05" ];
  Css.rule ".card-blurb" [ "font-size", "14px"; "opacity", "0.86" ];
  Css.rule ".spec" [ "display", "grid"; "grid-template-columns", "auto 1fr"; "gap", "8px 16px"; "font-size", "13px" ];
  Css.rule ".spec dt" [ "color", "var(--muted)"; "text-transform", "uppercase"; "letter-spacing", "0.14em"; "font-size", "10px"; "padding-top", "3px" ];
  Css.rule ".tag" [ "display", "inline-block"; "border", "1px solid var(--line)"; "padding", "0 7px"; "margin", "0 6px 4px 0"; "font-size", "12px" ];
  Css.rule ".status-dot" [
    "display", "inline-block"; "width", "8px"; "height", "8px"; "margin-right", "8px";
    "background", "var(--accent)"; "animation", "blink 1.6s steps(1) infinite";
  ];
  Css.rule ".card-link" [ "align-self", "flex-start"; "color", "var(--accent)"; "font-size", "13px"; "padding", "1px 4px"; "margin-left", "-4px" ];
  Css.rule ".card-link:hover" [ "background", "var(--accent)"; "color", "var(--on-accent)" ];
  Css.rule ".card-info .card-link:first-of-type" [ "margin-top", "auto" ];

  Css.rule "#links" [ "margin-bottom", "96px" ];

  Css.rule "#footer" [ "background", "var(--bar)"; "border-top", "1px solid var(--line)"; "padding", "40px 0 48px" ];
  Css.rule ".footer-inner" [ "display", "flex"; "flex-direction", "column"; "align-items", "center"; "gap", "18px"; "text-align", "center" ];
  Css.rule ".footer-mail" [ "color", "var(--accent)"; "letter-spacing", "0.12em"; "font-size", "14px" ];
  Css.rule ".footer-mail:hover" [ "background", "var(--accent)"; "color", "var(--on-accent)" ];
  Css.rule ".colophon" [ "max-width", "62ch"; "font-size", "11px"; "color", "var(--muted)" ];
  Css.rule ".colophon--quiet" [ "font-style", "italic"; "opacity", "0.6" ];

  Css.media "max-width: 1240px" [ ".vert", [ "display", "none" ] ];
  Css.media "max-width: 900px" [
    ".hero-grid", [ "grid-template-columns", "1fr" ];
    ".field-win", [ "position", "static" ];
    ".card-body", [ "grid-template-columns", "1fr" ];
    ".card-figure, .card:nth-child(even) .card-figure", [
      "order", "0"; "min-height", "260px"; "border-left", "0"; "border-right", "0"; "border-bottom", "1px solid var(--line)" ];
  ];
  Css.media "max-width: 600px" [
    "body", [ "font-size", "14px" ];
    ".win-body", [ "padding", "16px" ];
    ".card-info", [ "padding", "18px 16px" ];
    ".section-head", [ "margin-top", "64px"; "padding", "14px 16px" ];
    ".brand-sub, .clock-zone, .win-sub, .t-word", [ "display", "none" ];
    ".menu", [ "gap", "0" ];
    ".navbar", [ "letter-spacing", "0.06em" ];
    ".navbar-inner", [ "gap", "8px" ];
    ".menu-link", [ "padding", "3px 4px" ];
    ".menu-link::before, .menu-link::after", [ "margin", "0 1px" ];
    ".badges--links .badge", [ "width", "calc(50% - 7px)" ];
  ];
  Css.media "max-width: 420px" [
    ".brand-name, .clock", [ "display", "none" ];
  ];
  Css.media "prefers-reduced-motion: reduce" [
    "*, *::before, *::after", [ "animation", "none !important"; "transition", "none !important"; "scroll-behavior", "auto !important" ];
  ];
]

(* ---- Resume ---------------------------------------------------------- *)

let resume_styles = [
  Css.rule "#hero" [ "padding-top", "72px" ];
  Css.rule ".header-container, #resume-content" [ "width", "min(var(--wide), calc(100% - 32px))"; "max-width", "none" ];
  Css.rule ".header-container" [ "margin", "0 auto"; "padding", "24px 0 0" ];
  Css.rule ".header-container h1" [
    "font-family", "var(--display)"; "font-weight", "400"; "font-size", "clamp(40px, 7vw, 64px)"; "line-height", "1";
    "display", "inline-block"; "background", "var(--panel)"; "padding", "4px 14px"; "border-left", "4px solid var(--accent)";
  ];
  Css.rule ".header-container .content-text h2" [
    "font-size", "12px"; "text-transform", "uppercase"; "letter-spacing", "0.2em"; "color", "var(--accent)";
    "margin-top", "10px"; "font-weight", "400";
  ];
  Css.rule ".header-container .content-text h2::before" [ "content", "\"// \"" ];
  Css.rule "#resume-content" [
    "margin", "20px auto 96px";
    "padding", "28px 28px";
    "background", "var(--panel)";
    "border", "1px solid var(--line)";
    "box-shadow", "var(--hard)";
  ];
  Css.rule "#resume-content .content-text" [ "margin-bottom", "24px"; "color", "var(--muted)"; "font-size", "13px" ];
  Css.rule "#resume-content a" [ "color", "var(--accent)" ];
  Css.rule "#resume-content a:hover" [ "background", "var(--accent)"; "color", "var(--on-accent)" ];
  Css.rule ".section" [ "margin-bottom", "28px" ];
  Css.rule ".section h2" [
    "font-family", "var(--display)"; "font-weight", "400"; "font-size", "32px"; "line-height", "1.1";
    "padding-bottom", "6px"; "margin-bottom", "14px"; "border-bottom", "1px dashed var(--line)";
  ];
  Css.rule ".job, .education-item" [ "margin-bottom", "16px" ];
  Css.rule ".job-title, .degree" [ "font-weight", "600"; "font-size", "16px" ];
  Css.rule ".company, .school" [ "color", "var(--muted)"; "font-size", "13px" ];
  Css.rule ".date" [ "float", "right"; "color", "var(--muted)"; "font-size", "12px"; "font-variant-numeric", "tabular-nums" ];
  Css.rule ".job li::before" [ "content", "\"> \""; "color", "var(--accent)" ];
  Css.rule ".skills-section li" [ "padding", "6px 0" ];
  Css.rule ".skills-category" [ "color", "var(--accent)"; "font-weight", "600" ];
  Css.media "max-width: 600px" [
    "#resume-content", [ "padding", "18px 16px" ];
    ".date", [ "float", "none"; "display", "block" ];
  ];
]

let inject ts =
  let css_text = Css.render ts in
  let style = El.v (Jstr.v "style") [] in
  El.set_prop (El.Prop.jstr (Jstr.v "textContent")) (Jstr.v css_text) style;
  let head = Document.head G.document in
  El.set_children head (El.children head @ [ style ])

let inject_main () = inject (base @ chrome @ home)

let inject_resume () = inject (base @ chrome @ home @ resume_styles)
