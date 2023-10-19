open Raylib
open Constants

let draw_grid () =
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


