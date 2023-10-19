open Raylib
let starting_page () =
  Raylib.draw_text "Welcome to 2048!" 190 220 50 Color.brown;
  Raylib.draw_text "Press Space to Start" 240 300 30 Color.beige