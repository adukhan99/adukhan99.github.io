open Js_of_ocaml

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Logic.Style.inject_resume ();
    Logic.setup_logic ();
    Js._true
  )
