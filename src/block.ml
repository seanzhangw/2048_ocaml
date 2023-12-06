type block = {
  value : int;
  mutable current_pos : float * float; (* x, y *)
  mutable target_pos : float * float;
  mutable state : block_state;
}

and block_state =
  | Stationary
  | Moving of float (* movement progress *)
  | Merging

let empty_block (value : int) : block =
  {
    value = 0;
    current_pos = (0., 0.);
    target_pos = (0., 0.);
    state = Stationary;
  }

let place_block (value : int) (pos : int * int) : block =
  {
    value;
    current_pos = Constants.block_position_mapping pos;
    target_pos = (0., 0.);
    state = Stationary;
  }

let empty_board =
  [
    [ empty_block 0; empty_block 0; empty_block 0; empty_block 0 ];
    [ empty_block 0; empty_block 0; empty_block 0; empty_block 0 ];
    [ empty_block 0; empty_block 0; empty_block 0; empty_block 0 ];
    [ empty_block 0; empty_block 0; empty_block 0; empty_block 0 ];
  ]

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
            (fst block.current_pos) (fst block.target_pos) new_progress;
        block.state <- Moving new_progress
      end
  | Merging -> ()
(* Additional logic for merging animation *)

(* let draw_block block = *)
(* Draw the block based on block.current_pos *)
(* Change appearance if block.state is Merging *)
