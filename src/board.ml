open Raylib
open Constants
open Block

type block = Block.block

(* Draws the initial grid for the game board *)
let draw_init_grid () =
  let extended_square_size = square_size + spacing in
  let grid_size = num_squares * extended_square_size in
  let grid_x = (screen_width - grid_size + spacing) / 2 in
  let grid_y = ((screen_height - grid_size + spacing) / 2) + 40 in

  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the larger background square in dark gray *)
      Raylib.draw_rectangle (x - spacing) (y - spacing)
        (extended_square_size + spacing)
        (extended_square_size + spacing)
        Color.brown
    done
  done;

  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the smaller square with black lines *)
      Raylib.draw_rectangle x y square_size square_size Raylib.Color.beige
    done
  done;
  ()

(* Draws the home page button with the game title *)
let draw_home_page_button () =
  let mouse_x = Raylib.get_mouse_x () in
  let mouse_y = Raylib.get_mouse_y () in
  let is_mouse_over_button =
    mouse_x >= home_pos_x
    && mouse_x <= home_pos_x + home_width
    && mouse_y >= home_pos_y
    && mouse_y <= home_pos_y + home_height
  in

  let button_text =
    if is_mouse_over_button then Color.darkbrown else Color.brown
  in
  Raylib.draw_rectangle home_pos_x home_pos_x home_width home_height
    Color.raywhite;
  Raylib.draw_text "2048" home_pos_x home_pos_y home_text_size button_text

(* Draws the new game button with hover effects *)
let draw_new_game_button () =
  let mouse_x = Raylib.get_mouse_x () in
  let mouse_y = Raylib.get_mouse_y () in
  let is_mouse_over_button =
    mouse_x >= new_pos_x
    && mouse_x <= new_pos_x + new_width
    && mouse_y >= new_pos_y
    && mouse_y <= new_pos_y + new_height
  in
  let button_color =
    if is_mouse_over_button then Color.brown else Color.beige
  in
  let button_text =
    if is_mouse_over_button then Color.lightgray else Color.white
  in
  Raylib.draw_rectangle new_pos_x new_pos_y new_width new_height button_color;
  Raylib.draw_text "New Game" (new_pos_x + 26) (new_pos_y + 20) new_text_size
    button_text

(* Draws a single block on the game board with its numeric value *)
let draw_block (block : block) (size : int) =
  let x, y = block.current_pos in
  let x = int_of_float x in
  let y = int_of_float y in
  let value = Block.get_value block in
  if x != 0 && y != 0 && value != 0 then
    Raylib.draw_rectangle x y size size (color_mapping value);
  let show = string_of_int value in
  if value <> 0 then
    Raylib.draw_text show
      (x + (square_size / 2) - (Raylib.measure_text show 30 / 2))
      (y + (square_size / 2) - 15)
      30 Raylib.Color.white

(** Displays the current board data onto the board *)
let rec display_tiles_input (tiles : block list list) =
  let render_movement block = draw_block block square_size in
  let render_emerge block progress =
    if progress < initial_block_factor then
      draw_block block
        (int_of_float (float_of_int square_size *. initial_block_factor))
    else draw_block block (int_of_float (float_of_int square_size *. progress))
  in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j block ->
          match Block.get_state block with
          | Moving _ -> render_movement block
          | Emerging progress -> render_emerge block progress
          | _ -> render_movement block)
        row)
    tiles

(* Draws the main content of the game page, including title and buttons *)
let game_page () =
  draw_text "2048" 37 30 80 Color.brown;
  draw_home_page_button ();
  draw_new_game_button ();
  draw_init_grid ()
