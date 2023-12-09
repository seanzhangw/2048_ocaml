open Raylib
open Color
let rec lose_state () =
    draw_text "You lost!" 170 200 40 Color.brown;
    draw_text "Press escape to return to starting page. " 170 400 20 Color.beige;
    draw_text "Press s to start a new game. " 170 400 20 Color.beige;