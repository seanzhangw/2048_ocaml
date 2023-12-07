open Raylib
open Constants

let starting_page () =
  Raylib.draw_text "Welcome to 2048!" (screen_width - 620) (screen_height - 350)
    50 Color.brown;
  Raylib.draw_text "Press i for instructions" (screen_width - 595)
    (screen_height - 280) 30 Color.beige
