open Raylib
open Color
let rec instructions () =
    draw_text "These are the instructions..." 200 200 30 Color.black;

(* Return to the game loop if the Escape key is pressed *)