(* banner.ml — block-letter banners for <pre> headings, with a cast shadow.

   Glyphs are 5x5 bitmaps; add a letter by adding a row of five strings. *)

let font = [
  'A', [ " ### "; "#   #"; "#####"; "#   #"; "#   #" ];
  'D', [ "#### "; "#   #"; "#   #"; "#   #"; "#### " ];
  'E', [ "#####"; "#    "; "#### "; "#    "; "#####" ];
  'H', [ "#   #"; "#   #"; "#####"; "#   #"; "#   #" ];
  'K', [ "#   #"; "#  # "; "###  "; "#  # "; "#   #" ];
  'L', [ "#    "; "#    "; "#    "; "#    "; "#####" ];
  'N', [ "#   #"; "##  #"; "# # #"; "#  ##"; "#   #" ];
  'R', [ "#### "; "#   #"; "#### "; "#  # "; "#   #" ];
  'U', [ "#   #"; "#   #"; "#   #"; "#   #"; " ### " ];
  'X', [ "#   #"; " # # "; "  #  "; " # # "; "#   #" ];
  ' ', [ "   "; "   "; "   "; "   "; "   " ];
]

let full = "\xe2\x96\x88"     (* █ *)
let shadow = "\xe2\x96\x91"   (* ░ *)

(* Rasterise one word to rows of cells: [`Fill] where the bitmap is set,
   [`Shade] where the cell up-left is set (a cast shadow), else [`Blank]. *)
let cells word =
  let glyphs = List.map (fun ch -> List.assoc (Char.uppercase_ascii ch) font) (List.of_seq (String.to_seq word)) in
  let rows = List.init 5 (fun r -> String.concat " " (List.map (fun g -> List.nth g r) glyphs) ^ " ") in
  let width = String.length (List.hd rows) in
  let on r c = r >= 0 && r < 5 && c >= 0 && c < width && (List.nth rows r).[c] = '#' in
  List.init 6 (fun r -> List.init width (fun c ->
    if on r c then `Fill else if on (r - 1) (c - 1) then `Shade else `Blank))

(* The banner as runs of (is_shadow, text), so shadows can be styled. *)
let render words =
  let runs = ref [] in
  let push sh str = match !runs with
    | (sh', s') :: rest when sh' = sh -> runs := (sh, s' ^ str) :: rest
    | _ -> runs := (sh, str) :: !runs in
  List.iteri (fun wi word ->
    List.iteri (fun ri row ->
      if wi > 0 || ri > 0 then push false "\n";
      List.iter (function
        | `Fill -> push false full
        | `Shade -> push true shadow
        | `Blank -> push false " ") row)
      (cells word))
    words;
  List.rev !runs
