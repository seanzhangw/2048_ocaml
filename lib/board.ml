open Raylib
open Constants


let draw_init_grid () =
  let extended_square_size = square_size + spacing in (* includes the size of the square plus spacing *)

  let grid_size = num_squares * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size + spacing) / 2 in
  let grid_y = (screen_height - grid_size + spacing) / 2 + 20 in

  (* Draw dark gray squares including the margin area *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the larger background square in dark gray *)
      Raylib.draw_rectangle (x-spacing) (y-spacing) (extended_square_size + spacing) (extended_square_size + spacing) (Color.brown); (* dark gray *)
    done;
  done;

  (* Draw the regular squares on top of the dark gray background squares *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the smaller square with black lines *)
      Raylib.draw_rectangle x y square_size square_size Raylib.Color.beige;
    done;
  done;
  ()

  let draw_new_game_button () =
    (* Check if the mouse is over the button *)
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    let is_mouse_over_button = 
      mouse_x >= 600 && mouse_x <= 600 + 150 &&
      mouse_y >= 100 && mouse_y <= 100 + 50
    in
  
    (* Change the button's color based on mouse hover *)
    let button_color = if is_mouse_over_button then Color.lightgray else Color.gray in
  
    Raylib.draw_rectangle 600 100 150 50 button_color;
    Raylib.draw_text "New Game" (600 + 20) (600 + 15) 20 Color.black

  let score = "0"

  let game_page () = 
    draw_text "2048" 100 30 80 Color.brown;
    (* add button for instructions *)
    (* add score counter *)
    (* add button for new game *)
    draw_text "Score: " 600 30 30 Color.brown;
    draw_text score 600 60 30 Color.beige;
    draw_new_game_button ();

    draw_init_grid ();

