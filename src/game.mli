type game_state =
  | StartingPage
  | Game
  | InstructionsPage
  | Tetris

val score : int ref
val high_score : int ref
val last_move_time : float ref
val board : Block.block list list ref
val tetris_board : int array array ref
val is_tetris_initialized : bool ref
val setup : unit -> unit
val init_board : unit -> int array array
val init_tetris_board : unit -> unit
val starting_page_logic : unit -> game_state
val check_home_page_button_click : game_state -> game_state
val check_new_game_button_click : unit -> unit
val animate : float -> unit
val new_board : 'a array array -> 'a list -> unit
val tetris_game_logic : unit -> game_state
val new_board : 'a array array -> 'a list -> unit
val tetris_game_logic : unit -> game_state
val game_logic : float -> float -> game_state
val handle_move : float -> int -> unit
val instructions_logic : unit -> game_state
val main_loop : float -> game_state -> unit
