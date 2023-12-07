open Raylib
open Constants
open Block

(** Draws the board without the numbers *)
let draw_init_grid () =
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

(** Displays the durrent board data onto the board *)
let display_tiles_input (tiles : block list list) =
  (* print_block_list_list tiles; *)
  let color_of_value value =
    match value with
    | 2 -> Raylib.Color.skyblue
    | 4 -> Raylib.Color.blue
    | 8 -> Raylib.Color.darkblue
    | 16 -> Raylib.Color.darkpurple
    | 32 -> Raylib.Color.purple
    | 64 -> Raylib.Color.violet
    | 128 -> Raylib.Color.maroon
    | 256 -> Raylib.Color.magenta
    | 512 -> Raylib.Color.pink
    | 1024 -> Raylib.Color.orange
    | 2048 -> Raylib.Color.red
    | _ -> Raylib.Color.beige
  in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j block ->
          let x, y = block.current_pos in
          let x = int_of_float x in
          let y = int_of_float y in
          let value = block.value in
          if x != 0 && y != 0 then
            Raylib.draw_rectangle x y square_size square_size
              (color_of_value value);
          let show = string_of_int value in
          if value <> 0 then
            Raylib.draw_text show
              (x + (square_size / 2) - (Raylib.measure_text show 30 / 2))
              (y + (square_size / 2) - 15)
              30 Raylib.Color.white)
        row)
    tiles

(** Calls all of the neccesary functions that displays the game page*)
let game_page () =
  draw_text "2048" 37 30 80 Color.brown;
  draw_home_page_button ();
  (* add button for instructions *)
  (* add button for new game *)
  draw_new_game_button ();
  draw_init_grid ()

(* display_tiles !board *)

(* display_tiles board; *)
