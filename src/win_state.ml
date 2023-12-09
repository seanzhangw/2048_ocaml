open Raylib
open Color

let win_state () =
  draw_text "You won!" 170 200 40 Color.brown;
  draw_text "Press s to start a new game" 170 270 20 Color.beige;
  draw_text "Press d to continue playing" 170 300 20 Color.beige
