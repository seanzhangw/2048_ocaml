open Raylib
open Constants

let draw_grid () =
  let extended_square_size = square_size + spacing in (* includes the size of the square plus spacing *)

  let grid_size = num_squares * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size + spacing) / 2 in
  let grid_y = (screen_height - grid_size + spacing) / 2 + 20 in

  (* Set location for dark gray squares including the margin area *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the larger background square in dark gray *)
      Raylib.draw_rectangle (x-spacing) (y-spacing) (extended_square_size + spacing) (extended_square_size + spacing) (Color.brown); (* dark gray *)
    done;
  done;

  (* Set location for the regular squares on top of the dark gray background squares *)
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let x = grid_x + (i * extended_square_size) in
      let y = grid_y + (j * extended_square_size) in

      (* Draw the smaller square with black lines *)
      Raylib.draw_rectangle x y square_size square_size Raylib.Color.beige;
    done;
  done;
  ()

  let score = "0"

  let game_page () = 
    draw_text "2048" 100 30 80 Color.brown;
    (* add button for instructions *)
    (* add score counter *)
    (* add button for new game *)
    draw_text "Score: " 600 30 30 Color.brown;
    draw_text score 600 60 30 Color.beige;
    draw_grid ();
  let display_tiles ()= 
    let tiles = [[0; 0; 2; 0]; [0; 0; 2; 0]; [0; 0; 2; 0]; [0; 0; 0; 0]] in
    let extended_square_size = square_size + spacing in (* includes the size of the square plus spacing *)
  
    let grid_size = num_squares * extended_square_size in
  
    (* Calculate positions for centering the grid on the screen *)
    let grid_x = (screen_width - grid_size + spacing) / 2 in
    let grid_y = (screen_height - grid_size + spacing) / 2 + 20 in
      let rec nth list index = 
        match list with 
        | [] -> failwith "Out of bounds"
        | head :: tail -> if index = 0 then head
        else nth tail (index - 1)
      in
      for i = 0 to 3 do
        let row = nth tiles i in
          for j = 0 to 3 do
            let x = grid_x + (i * extended_square_size) in
            let y = grid_y + (j * extended_square_size) in
            (* let head lst =
              match lst with
              | hd :: _ -> hd   (* Return the head if the list is non-empty *)
              | [] -> 0    
            in *)
            let value = nth row j in
            let show = string_of_int value in
              (* Raylib.draw_text show x y 20 Color.black; *)
              Raylib.draw_text show (x + (square_size / 2) - 17) (y + (square_size / 2) - 30) 70 Color.white;
          done
      done


  (* let display_tiles (tiles : int list list) () = 
  let extended_square_size = square_size + spacing in (* includes the size of the square plus spacing *)

  let grid_size = num_squares * extended_square_size in

  (* Calculate positions for centering the grid on the screen *)
  let grid_x = (screen_width - grid_size + spacing) / 2 in
  let grid_y = (screen_height - grid_size + spacing) / 2 + 20 in
    let rec nth list index = 
      match list with 
      | [] -> failwith "Out of bounds"
      | head :: tail -> if index = 0 then head
      else nth tail (index - 1)
    in
    for i = 0 to 3 do
      let row = nth tiles i in
        for j = 0 to 3 do
          let x = grid_x + (i * extended_square_size) in
          let y = grid_y + (j * extended_square_size) in
          (* let head lst =
            match lst with
            | hd :: _ -> hd   (* Return the head if the list is non-empty *)
            | [] -> 0    
          in *)
          let value = nth row j in
          let show = string_of_int value in
            Raylib.draw_text show (x + (square_size / 2) - 17) (y + (square_size / 2) - 30) 70 Color.white;
        done
    done


 *)
