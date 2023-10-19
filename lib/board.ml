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

  (* this was an example display for testing*)
  let display_tiles ()= 
    let tiles = [[0; 2; 4; 8]; [16; 32; 64; 128]; [256; 512; 1024; 2048]; [0; 0; 0; 0]] in
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
        let col = nth tiles i in
          for j = 0 to 3 do
            let x = grid_x + (j * extended_square_size) in
            let y = grid_y + (i * extended_square_size) in
            let value = nth col j in
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
            Raylib.draw_rectangle x y square_size square_size (color_of_value value);

            let show = string_of_int value in
            if value <> 0 then
          Raylib.draw_text show (x + (square_size / 2) - ((Raylib.measure_text show 30) / 2)) 
          (y + (square_size / 2) - 15) 30 Raylib.Color.white;
          done
      done 

      let display_tiles_input (tiles : int list list) = 
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
            let col = nth tiles i in
              for j = 0 to 3 do
                let x = grid_x + (j * extended_square_size) in
                let y = grid_y + (i * extended_square_size) in
                let value = nth col j in
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
                Raylib.draw_rectangle x y square_size square_size (color_of_value value);
    
                let show = string_of_int value in
                if value <> 0 then
              Raylib.draw_text show (x + (square_size / 2) - ((Raylib.measure_text show 30) / 2)) 
              (y + (square_size / 2) - 15) 30 Raylib.Color.white;
              done
          done

  let score = "0"

  let game_page () = 

    draw_text "2048" 100 30 80 Color.brown;
    (* add button for instructions *)
    (* add score counter *)
    (* add button for new game *)
    draw_text "Score: " 600 30 30 Color.brown;
    draw_text score 600 60 30 Color.beige;
    draw_grid ();
    display_tiles ();
  



