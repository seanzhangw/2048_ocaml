open Raylib
open Color
open Raylib

let starting_page () =
  Raylib.draw_text "Welcome to 2048!" 190 220 50 Color.brown;
  Raylib.draw_text "Press space to start" 240 300 30 Color.beige;
  Raylib.draw_text "Press i for instructions" 220 350 30 Color.beige

let instructions () =
  draw_text "Instructions:" 170 200 40 Color.brown;
  draw_text "Use your arrow keys to move the tiles." 170 270 20 Color.beige;
  draw_text "Tiles with the same number merge into" 170 300 20 Color.beige;
  draw_text "one when they touch. Add them up to reach 2048!" 170 330 20
    Color.beige;
  draw_text "Press s to start game! " 170 400 20 Color.beige
