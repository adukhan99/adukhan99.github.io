(* projects.ml — the work shown on the home page.

   To add a project, append a record to [all]. Cards are numbered, laid out
   and animated automatically; [visual] picks the live ASCII figure:

     Dna_bound             bent double helix with two bound proteins
     Agent_graph           orchestrator hub, agents, travelling messages
     Torus                 a shaded torus
     Wireframe `Cube       (or `Octahedron / `Icosahedron) a spinning solid

   New figures live in scene.ml. *)

module Scene = Logic.Scene

type t = {
  slug : string;                    (* shown in the card's title bar *)
  title : string;
  blurb : string;
  org : string;
  stack : string list;
  status : string option;
  links : (string * string) list;   (* label, url *)
  visual : Scene.visual;
  caption : string;                 (* figure caption under the visual *)
}

let all = [
  { slug = "nucleoid.sim";
    title = "Prokaryotic Nucleoid Associated Proteins and the ProU70 Promoter Sequence";
    blurb = "Molecular dynamics of nucleoid-associated proteins binding and shaping DNA at the proU promoter.";
    org = "University of York \xc2\xb7 Noy Lab";
    stack = [ "Amber"; "Python"; "R" ];
    status = Some "research in progress";
    links = [];
    visual = Scene.Dna_bound;
    caption = "fig. 1 \xe2\x80\x94 duplex bent by two bound NAPs" };
  { slug = "caravan.ml";
    title = "Caravan \xe2\x80\x94 Multi-agent framework";
    blurb = "A typed, self-documenting agentic CLI harness and LLM orchestration framework for OCaml, built on OCaml 5 effects and Eio, and light enough for HPC nodes and containers.";
    org = "Personal project \xc2\xb7 open source";
    stack = [ "OCaml"; "Eio"; "AI" ];
    status = None;
    links = [ ("repository", "https://github.com/adukhan99/Caravan");
              ("documentation", "https://adukhan99.github.io/Caravan/") ];
    visual = Scene.Agent_graph;
    caption = "fig. 2 \xe2\x80\x94 orchestrator dispatching across agents" };
]
