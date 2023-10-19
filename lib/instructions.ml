open Raylib
open Color
let rec instructions () =
    draw_text "Instructions" 200 200 30 Color.brown;
    draw_text "These are the instructions...." 200 240 20 Color.beige;

(* Return to the game loop if the Escape key is pressed *)