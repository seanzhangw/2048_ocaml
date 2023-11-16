open Raylib
let starting_page () =
  Raylib.draw_text "Welcome to 2048!" 190 220 50 Color.brown;
  Raylib.draw_text "Press space to start" 240 300 30 Color.beige;
  Raylib.draw_text "Press i for instructions" 220 350 30 Color.beige

