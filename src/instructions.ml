open Raylib
open Color

let instructions () =
    draw_text "Instructions:" 170 200 40 Color.brown;
    draw_text "Use your arrow keys to move the tiles." 170 240 20 Color.beige;
    draw_text "Tiles with the same number merge into" 170 270 20 Color.beige;
    draw_text "one when they touch. Add them up to reach 2048!" 170 300 20 Color.beige;
    draw_text "Press s to start game! " 170 330 20 Color.beige;


