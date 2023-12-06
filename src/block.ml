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

let print_block_list_list board =
  List.iteri
    (fun i row ->
      List.iteri
        (fun j block ->
          Printf.printf "Block at (%d, %d) - Value: %d, Position: (%f, %f)\n" i
            j block.value (fst block.current_pos) (snd block.current_pos))
        row)
    board

let empty_board =
  [
    [
      place_block 2 (0, 0);
      place_block 2 (1, 0);
      place_block 2 (2, 0);
      place_block 2 (3, 0);
    ];
    [ place_block 2 (0, 1); empty_block 0; empty_block 0; empty_block 0 ];
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
