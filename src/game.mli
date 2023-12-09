(** File controlling the game state and creating a game instance. *)

(** Possible game states the game instance can be in. *)
type game_state =
  | StartingPage
  | Game
  | InstructionsPage
  | Lost
  | Won
  | ContinuePlaying

val score : int ref
(** Reference to the current score in the game. *)

val high_score : int ref
(** Reference to the highest score achieved in the game. *)

val last_move_time : float ref
(** Reference to the timestamp of the last move in the game. *)

val board : Block_logic.block list list ref
(** Reference to the game board, represented as a list of blocks. *)

val setup : unit -> unit
(** Initializes the Raylib window with the specified size and frame rate. *)

val reset : unit -> unit
(** Reset the board and returns the score to 0*)

val init_board : unit -> int array array
(** Initializes a placeholder game board represented as a 2D array. *)

val starting_page_logic : unit -> game_state
(** Handles the logic for the starting page, checking for key input to progress
    to instructions or the game state. Returns the next game state. *)

val check_home_page_button_click : unit -> bool
(** Checks for the home page button click and resets the board if clicked.
    Returns the next game state. *)

val animate : float -> unit
(** Animates the game board based on the elapsed time. *)

val game_logic : float -> float -> game_state
(** Handles the game logic, checking for key input, button clicks, and updating
    the board. Returns the next game state. *)

val handle_move : float -> int -> game_state
(** Handles the movement of the game board based on user input and updates the
    game state accordingly. *)

val instructions_logic : unit -> game_state
(** Handles the logic for the instruction page, checking for key input to return
    to the start page or begin the game. Returns the next game state. *)

val main_loop : float -> game_state -> unit
(** Main control loop of the game, executing different logic blocks based on the
    current game state. *)

val check_foldable_row : Block_logic.block list -> bool
(** Checks if a row is foldable. *)

val check_row_foldable : Block_logic.block list list -> bool
(** Checks if a board is foldable, row-wise. *)

val check_col_foldable : Block_logic.block list list -> bool
(** Checks if a board is foldable, column-wise. *)

val check_foldable : Block_logic.block list list -> bool
(** Checks if a board is foldable. *)

val find_2048 : Block_logic.block list list -> bool
(** Checks if there is a 2048 value present. *)

val lost_state : unit -> game_state
(** Returns to lose state *)

val won_state : unit -> game_state
(** Returns to win state *)

val continue_playing_state : unit -> game_state
(** Returns to continue playing state *)
