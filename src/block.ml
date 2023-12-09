open Constants

module Block = struct
  type block_state =
    | Stationary
    | Moving of float
    | Merging
    | Emerging of float
    | Blank

  type block = {
    mutable value : int;
    mutable current_pos : float * float;
    mutable target_pos : float * float;
    mutable state : block_state;
  }
 (** Convert a block to its string representation. *)
  let to_string block = Printf.sprintf "%d" block.value
    
  (** Check if two blocks are equal based on their values and positions. *)
  let block_equal b1 b2 = b1.value = b2.value && b1.current_pos = b2.current_pos

  (** Get the current grid position of a block. *)
  let get_current_grid_pos (block : block) =
    block_board_mapping block.current_pos
  
  (** Set the current grid position of a block. *) 
  let set_current_grid_pos (block : block) (col : int) (row : int) =
    block.current_pos <- block_position_mapping (col, row)
  
  (** Get the target grid position of a block. *)
  let get_target_grid_pos (block : block) = block_board_mapping block.target_pos
  
  (** Set the target grid position of a block. *)
  let set_target_grid_pos (block : block) (col : int) (row : int) =
    block.target_pos <- block_position_mapping (col, row)

  (** Get the current actual position (float) of a block. *)
  let get_current_actual_pos (block : block) = block.current_pos
  
  (** Get the target actual position (float) of a block. *)
  let get_target_actual_pos (block : block) = block.target_pos
  
  (** Get the value of a block. *)
  let get_value (block : block) = block.value
  
  (** Set the value of a block. *)
  let set_value (block : block) (value : int) = block.value <- value
  
  (** Get the state of a block. *)
  let get_state (block : block) = block.state

  (** Set the state of a block. *)
  let set_state (block : block) (state : block_state) = block.state <- state

(** Create a stationary block with a given value and grid position. *)
  let place_block (value : int) (pos : int * int) : block =
    {
      value;
      current_pos = block_position_mapping pos;
      target_pos = (0., 0.);
      state = Stationary;
    }

(** Create a blank block with a given value and grid position. *)
  let place_blank_block (value : int) (pos : int * int) : block =
    {
      value;
      current_pos = block_position_mapping pos;
      target_pos = (0., 0.);
      state = Blank;
    }

(** Print the details of each block in a board. *)
  let print_block_list_list board =
    List.iteri
      (fun i row ->
        List.iteri
          (fun j block ->
            Printf.printf "Block at (%d, %d) - Value: %d, Position: (%f, %f)\n"
              i j block.value (fst block.current_pos) (snd block.current_pos))
          row)
      board

  (* Function to update a block's position *)
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
    | Merging -> ()
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

  (** An empty game board. *)
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
