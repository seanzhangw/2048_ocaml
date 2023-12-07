open Raylib
open Constants

let rows = 5 (* 5 rows for the board *)
let columns = 4 (* 4 columns for the board *)

(* Initialize the board as a 5x4 mutable matrix with all cells set to 0 *)
let init_tetris_board () = Array.init rows (fun _ -> Array.make columns 0)

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

(* Function to create a random block in the first row and store it in the
   board *)
let generate_random_block () =
  let value = if Random.int 2 = 0 then 2 else 4 in
  let column = Random.int columns in
  let block = Array.make columns 0 in
  block.(column) <- value;
  Array.to_list block

let can_move_down board block_x block_y =
  (* Check if any cells below the block are not empty *)
  let can_move = ref true in

  (* Initialize can_move to true *)
  for i = block_x to block_x + 1 do
    for j = block_y + 1 to block_y + 2 do
      if board.(j).(i) <> 0 then
        can_move := false (* Set can_move to false if there's an obstacle *)
    done
  done;

  can_move (* Return the final value of can_move *)

let move_block_down board block_x block_y block_value =
  let can_move = can_move_down board block_x block_y in
  if !can_move then begin
    (* Move the block down by modifying the game board *)
    (* Clear the current block's position *)
    for i = block_x to block_x + 1 do
      for j = block_y to block_y + 1 do
        board.(j).(i) <- 0
      done
    done;

    (* Update the block's position to move it down *)
    for i = block_x to block_x + 1 do
      for j = block_y + 1 to block_y + 2 do
        board.(j).(i) <- block_value
      done
    done
  end
  else begin
    (* Block has reached the bottom or another block, generate a new block *)
    let new_block = generate_random_block () in
    (* Add the new block to the top row of the grid *)
    for i = 0 to columns - 1 do
      board.(0).(i) <- List.nth new_block i
    done
  end

let draw_init_tetris_grid () =
  let extended_square_size = square_size + spacing in

  (* includes the size of the square plus spacing *)
  let grid_size_i = num_squares * extended_square_size in
  let grid_size_j = (num_squares + 1) * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size_i + spacing) / 2 in
  let grid_y = ((screen_height - grid_size_j + spacing) / 2) + 40 in

  (* Set location for dark gray squares including the margin area *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares do
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
    for j = 1 to num_squares do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the smaller square with black lines *)
      Raylib.draw_rectangle x y square_size square_size Raylib.Color.beige
    done
  done;
  ()

let draw_board (board : int array array) =
  (* Constants for the Tetris board *)
  let num_rows = 5 in
  let num_columns = 4 in

  (* Spacing between squares *)
  let extended_square_size = square_size + spacing in

  (* Calculate positions for centering the grid on the screen *)
  let grid_size_i = num_squares * extended_square_size in
  let grid_size_j = (num_squares + 1) * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size_i + spacing) / 2 in
  let grid_y = ((screen_height - grid_size_j + spacing) / 2) + 40 in

  (* Loop through the 2D array and draw each block based on its value *)
  for i = 0 to num_rows - 1 do
    for j = 0 to num_columns - 1 do
      (* Determine the position (x, y) for the current block *)
      let x = grid_x + (j * (square_size + spacing)) in
      let y = grid_y + (i * (square_size + spacing)) in

      (* Get the value (0, 2, or 4) from the board *)
      let value = board.(i).(j) in

      (* Determine the color based on the value *)
      let block_color =
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
        | _ -> Raylib.Color.raywhite
      in

      (* Draw the block as a rectangle with the calculated position and color *)
      Raylib.draw_rectangle x y square_size square_size block_color;

      (* Draw the value in the center of the block *)
      if value <> 0 then
        let show = string_of_int value in
        let text_x =
          x + (square_size / 2) - (Raylib.measure_text show 30 / 2)
        in
        let text_y = y + (square_size / 2) - (30 / 2) in
        Raylib.draw_text show text_x text_y 30 Raylib.Color.white
    done
  done

(* display the tetris game page *)
let tetris_page () =
  draw_text "2048" 37 30 80 Color.brown;
  draw_home_page_button ();

  draw_new_game_button ();
  draw_init_tetris_grid ()

let update_board board =
  let delay_seconds = 1 in

  (* Draw the Tetris board *)
  draw_board board;

  (* Sleep for 1 second to introduce the delay *)
  Unix.sleep delay_seconds
