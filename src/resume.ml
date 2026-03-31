open Js_of_ocaml

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Logic.setup_logic ();
    Js._true
  )
