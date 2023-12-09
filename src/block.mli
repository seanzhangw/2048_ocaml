type block = {
  value : int;
  mutable current_pos : float * float;
  mutable target_pos : float * float;
  mutable state : block_state;
}

and block_state =
  | Stationary
  | Moving of float
  | Merging
  | Emerging of float
  | Blank

val place_block : int -> int * int -> block
val print_block_list_list : block list list -> unit
val empty_board : block list list
val update_block : block -> float -> unit
