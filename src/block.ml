open Constants

module Block = struct
  type block_state =
    | Stationary
    | Moving of float
    | Merging of float
    | Emerging of float
    | Blank

  type block = {
    mutable value : int;
    mutable current_pos : float * float;
    mutable target_pos : float * float;
    mutable state : block_state;
  }

  let to_string block = Printf.sprintf "%d" block.value
  let block_equal b1 b2 = b1.value = b2.value && b1.current_pos = b2.current_pos

  let get_current_grid_pos (block : block) =
    block_board_mapping block.current_pos

  let set_current_grid_pos (block : block) (col : int) (row : int) =
    block.current_pos <- block_position_mapping (col, row)

  let get_target_grid_pos (block : block) = block_board_mapping block.target_pos

  let set_target_grid_pos (block : block) (col : int) (row : int) =
    block.target_pos <- block_position_mapping (col, row)

  let get_current_actual_pos (block : block) = block.current_pos
  let get_target_actual_pos (block : block) = block.target_pos
  let get_value (block : block) = block.value
  let set_value (block : block) (value : int) = block.value <- value
  let get_state (block : block) = block.state
  let set_state (block : block) (state : block_state) = block.state <- state

  let place_block (value : int) (pos : int * int) : block =
    {
      value;
      current_pos = block_position_mapping pos;
      target_pos = (0., 0.);
      state = Stationary;
    }

  let place_blank_block (value : int) (pos : int * int) : block =
    {
      value;
      current_pos = block_position_mapping pos;
      target_pos = (0., 0.);
      state = Blank;
    }

  let print_block_list_list board =
    List.iteri
      (fun i row ->
        List.iteri
          (fun j block ->
            Printf.printf "Block at (%d, %d) - Value: %d, Position: (%f, %f)\n"
              i j block.value (fst block.current_pos) (snd block.current_pos))
          row)
      board

  let update_block block delta_time =
    let interpolate x_cur x_target y_cur y_target progress =
      ( x_cur +. ((x_target -. x_cur) *. progress),
        y_cur +. ((y_target -. y_cur) *. progress) )
    in
    match block.state with
    | Stationary -> ()
    | Moving progress ->
        let new_progress = progress +. (Constants.block_speed *. delta_time) in
        if new_progress >= 1.0 then begin
          block.current_pos <- block.target_pos;
          block.state <- Stationary
        end
        else begin
          block.current_pos <-
            interpolate (fst block.current_pos) (fst block.target_pos)
              (snd block.current_pos) (snd block.target_pos) new_progress;
          block.state <- Moving new_progress
        end
    | Merging progress ->
        let new_progress =
          progress +. (Constants.block_scaling *. delta_time)
        in
        if new_progress >= 1.0 then begin
          block.state <- Stationary
        end
        else begin
          block.state <- Merging new_progress
        end
    | Emerging progress ->
        let new_progress =
          progress +. (Constants.block_scaling *. delta_time)
        in
        if new_progress >= 1.0 then begin
          block.state <- Stationary
        end
        else begin
          block.state <- Emerging new_progress
        end
    | Blank -> ()

  let equal (old_board : block list list) (new_board : block list list) =
    let blocks_equal b1 b2 = b1.value = b2.value in
    let rows_equal row1 row2 =
      try List.for_all2 blocks_equal row1 row2
      with Invalid_argument _ -> false
    in
    try List.for_all2 rows_equal old_board new_board
    with Invalid_argument _ -> false

  let empty_board =
    [
      [
        place_block 0 (0, 0);
        place_block 0 (1, 0);
        place_block 0 (2, 0);
        place_block 0 (3, 0);
      ];
      [
        place_block 0 (0, 1);
        place_block 0 (1, 1);
        place_block 0 (2, 1);
        place_block 0 (3, 1);
      ];
      [
        place_block 0 (0, 2);
        place_block 0 (1, 2);
        place_block 0 (2, 2);
        place_block 0 (3, 2);
      ];
      [
        place_block 0 (0, 3);
        place_block 0 (1, 3);
        place_block 0 (2, 3);
        place_block 0 (3, 3);
      ];
    ]
end
