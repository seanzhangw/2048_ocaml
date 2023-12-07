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
    (screen_width - 678) (screen_height - 340) 20 Color.beige;
  (*Game instrcutions 2*)
  draw_text "Game Mode 2: Tetris Ver. (t to start)" (screen_width - 680)
    (screen_height - 290) 20 Color.brown;
  draw_text "The gameplay is same like the original Tetris game."
    (screen_width - 680) (screen_height - 240) 20 Color.beige;
  draw_text "The blocks containing number falls from the top."
    (screen_width - 680) (screen_height - 210) 20 Color.beige;
  draw_text "The player needs to collapse the falling blocks in"
    (screen_width - 680) (screen_height - 180) 20 Color.beige;
  draw_text "left, right or downward direction. The aim is" (screen_width - 675)
    (screen_height - 150) 20 Color.beige;
  draw_text "to get to the 2048 tile!" (screen_width - 675)
    (screen_height - 120) 20 Color.beige
