open Raylib
open Constants

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
