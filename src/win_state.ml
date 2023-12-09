open Raylib
open Color

let win_state () =
    let lines = [
      ("You won!", 40);
      ("Press escape to exit.", 20);
      ("Press s to start a new game.", 20);
      ("Press d to continue playing.", 20)
    ] in
  
    let base_x = 170 in
    let base_y = 200 in
    let line_height = 40 in 
  
    List.iteri (fun i (text, size) ->
      draw_text text (base_x) (base_y + i * line_height) size Color.beige
    ) lines  
