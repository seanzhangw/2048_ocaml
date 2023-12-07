open Raylib
open Constants

type block = int (* Blocks are numbers 2 to 4 *)
type board = block array array (* 4x4 board *)

(* Function to initialize an empty board *)
let init_board () = Array.make_matrix 4 4 0

(* Draw new home page button*)
let draw_home_page_button () =
  (* Check if the mouse is over the button *)
  let mouse_x = Raylib.get_mouse_x () in
  let mouse_y = Raylib.get_mouse_y () in
  let is_mouse_over_button =
    mouse_x >= 37 && mouse_x <= 37 + 184 && mouse_y >= 30 && mouse_y <= 30 + 56
  in

  (* Change the button's color based on mouse hover *)
  let button_text =
    if is_mouse_over_button then Color.darkbrown else Color.brown
  in
  Raylib.draw_rectangle 37 38 184 56 Color.raywhite;
  Raylib.draw_text "2048" 37 30 80 button_text

(** Draws new game button *)
let draw_new_game_button () =
  (* Check if the mouse is over the button *)
  let mouse_x = Raylib.get_mouse_x () in
  let mouse_y = Raylib.get_mouse_y () in
  let is_mouse_over_button =
    mouse_x >= 615
    && mouse_x <= 615 + 150
    && mouse_y >= 37
    && mouse_y <= 37 + 58
  in

  (* Change the button's color based on mouse hover *)
  let button_color =
    if is_mouse_over_button then Color.brown else Color.beige
  in
  let button_text =
    if is_mouse_over_button then Color.lightgray else Color.white
  in
  Raylib.draw_rectangle 615 37 150 58 button_color;
  Raylib.draw_text "New Game" (615 + 26) (37 + 20) 20 button_text

(* Function to generate a new block *)
let new_block () =
  if Random.int 1 == 0 then 2
  else 4 (* Generates a block with a value of either 2 or 4 *)

let draw_init_tetris_grid () =
  let extended_square_size = square_size + spacing in

  (* includes the size of the square plus spacing *)
  let grid_size = num_squares * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size + spacing) / 2 in
  let grid_y = ((screen_height - grid_size + spacing) / 2) + 40 in

  (* Set location for dark gray squares including the margin area *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the larger background square in dark gray *)
      Raylib.draw_rectangle (x - spacing) (y - spacing)
        (extended_square_size + spacing)
        (extended_square_size + spacing)
        Color.brown
      (* dark gray *)
    done
  done;

  (* Set location for the regular squares on top of the dark gray background
     squares *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the smaller square with black lines *)
      Raylib.draw_rectangle x y square_size square_size Raylib.Color.beige
    done
  done;
  ()

(* display the tetris game page *)
let tetris_page () =
  draw_text "2048" 37 30 80 Color.brown;
  draw_home_page_button ();
  (* add button for instructions *)
  (* add button for new game *)
  draw_new_game_button ();
  draw_init_tetris_grid ()

(* Function to update the board with block movement *)
let update_board board =
  (* Logic to move blocks down and handle collisions *)
  (* ... *)
  ()

(* Add more functions as needed for handling game logic *)
