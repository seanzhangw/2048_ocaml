(** The [Block] module represents a 2048 tile. *)

(** A [Block] represents a 2048 tile. *)
module Block : sig
  (** [block_state] represents the current behavior of the block. The block can
      either be [Stationary], [Moving], [Merging], [Emerging], or [Blank]*)
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
  (** [block] is the type of 2048 tiles. It contains information about the
      block's value, the current position, the target position, and the block's
      state. *)

  val to_string : block -> string
  (** [to_string] returns the string that represents the value of the passed in
      block. *)

  val block_equal : block -> block -> bool
  (** [block_equal] returns if two blocks are equivalent by comparing their
      values and their current positions. *)

  val get_current_grid_pos : block -> int * int
  (** [get_current_grid_pos] returns the current position of the passed in block
      in grid coordinate format. *)

  val set_current_grid_pos : block -> int -> int -> unit
  (** [set_current_grid_pos] sets the current position of the passed in block in
      grid coordinate format. *)

  val get_target_grid_pos : block -> int * int
  (** [get_target_grid_pos] returns the target position of the passed in block
      in grid coordinate format. *)

  val set_target_grid_pos : block -> int -> int -> unit
  (** [set_target_grid_pos] sets the target position of the passed in block in
      grid coordinate format. *)

  val get_current_actual_pos : block -> float * float
  (** [get_current_actual_pos] returns the current position of the passed in
      block in specific coordinate format. *)

  val get_target_actual_pos : block -> float * float
  (** [get_target_actual_pos] returns the target position of the passed in block
      in the specific coordinate format. *)

  val get_value : block -> int
  (** [get_value] returns the value of the passed in block. *)

  val set_value : block -> int -> unit
  (** [set_value] sets the value of the passed in block to a new value. *)

  val get_state : block -> block_state
  (** [get_state] returns the state of the passed in block. *)

  val set_state : block -> block_state -> unit
  (** [set_state] sets the state of the passed in block to a new state. *)

  val place_block : int -> int * int -> block
  (** [place_block] returns a block with a passed in value and current position. *)

  val place_blank_block : int -> int * int -> block
  (** [place_blank_block] returns a block with a passed in value and current
      position with the [Blank] state. *)

  val print_block_list_list : block list list -> unit
  (** [print_block_list_list] prints the current board by outputting the values
      and current grid positions of all the blocks in the board. *)

  val update_block : block -> float -> unit
  (** [update_block] steps and keeps track of the animation progress of the
      block. *)

  val equal : block list list -> block list list -> bool
  (** [equal] compares if the block list list contains the same values in the
      correct positions. *)

  val empty_board : block list list
  (** [empty_board] is the blank board full of blocks with a value of 0. *)
end
