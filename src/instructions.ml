open Raylib
open Constants
open Color

let rec instructions () =
  draw_text "Instructions:" (screen_width - 680) (screen_height - 520) 40
    Color.darkbrown;
  (*Game instructions 1*)
  draw_text "Game Mode 1: Original (o to start)" (screen_width - 680)
    (screen_height - 450) 20 Color.brown;
  draw_text "Use your arrow keys to move the tiles." (screen_width - 680)
    (screen_height - 400) 20 Color.beige;
  draw_text "Tiles with the same number merge into" (screen_width - 680)
    (screen_height - 370) 20 Color.beige;
  draw_text "one when they touch. Add them up to reach 2048!"
    (screen_width - 678) (screen_height - 340) 20 Color.beige
