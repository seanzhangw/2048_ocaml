(** This file contains constant values that define behavior within the game. *)

val screen_width : int
(** [screen_width] is the width of the window opened by RayLib. *)

val screen_height : int
(** [screen_height] is the height of the window opened by RayLib. *)

val num_squares : int
(** [num_squares] is the number of squares in each row of the grid. *)

val square_size : int
(** [square_size] is the size of each square in the grid. *)

val spacing : int
(** [spacing] is the distance between each square in the grid. *)

val fps : int
(** [fps] is the frames per second of the RayLib window. *)

val home_pos_x : int
(** [home_pos_x] is the x-coordinate of the home button. *)

val home_width : int
(** [home_width] is the width of the home button. *)

val home_pos_y : int
(** [home_pos_y] is the y-coordinate of the home button. *)

val home_height : int
(** [home_height] is the width of the home button. *)

val home_text_size : int
(** [home_text_size] is the font size of the home button label. *)

val new_pos_x : int
(** [new_pos_x] is the x-coordinate of the new game button. *)

val new_width : int
(** [new_width] is the width of the new game button. *)

val new_pos_y : int
(** [new_pos_y] is the y-coordinate of the new game button. *)

val new_height : int
(** [new_height] is the height of the new game button. *)

val new_text_size : int
(** [new_text_size] is the font size of the new game button label. *)

val score_label_pos_x : int
(** [score_label_pos_x] is the x-coordinate of the score label. *)

val score_label_pos_y : int
(** [score_label_pos_y] is the y-coordinate of the score label. *)

val score_label_size : int
(** [score_label_size] is the font size of the score label. *)

val score_pos_x : int
(** [score_pos_x] is the x-coordinate of the score display. *)

val score_pos_y : int
(** [score_pos_y] is the y-coordinate of the score display. *)

val score_size : int
(** [score_size] is the font size of the score display. *)

val hs_label_pos_x : int
(** [hs_label_pos_x] is the x-coordinate of the high score label. *)

val hs_label_pos_y : int
(** [hs_label_pos_y] is the y-coordinate of the high score label. *)

val hs_label_size : int
(** [hs_label_size] is the font size of the high score label. *)

val hs_pos_x : int
(** [hs_pos_x] is the x-coordinate of the high score display. *)

val hs_pos_y : int
(** [hs_pos_y] is the y-coordinate of the high score display. *)

val hs_size : int
(** [hs_size] is the font size of the high score display. *)

val encouragement_text_size : int
(** [encouragement_text_size] is the font size of the encouragement test. *)

val color_mapping : int -> Raylib.Color.t
(** [color_mapping] is a function that maps an integer (block value) to a color. *)

val move_left : int
(** [move_left] is the key code for moving left. *)

val move_right : int
(** [move_right] is the key code for moving right. *)

val move_up : int
(** [move_up] is the key code for moving up. *)

val move_down : int
(** [move_down] is the key code for moving down. *)

val file_path : string
(** [file_path] is the path to the file used for saving/loading game data. *)

val move_cooldown : float
(** [move_cooldown] is the cooldown time in seconds for making a move. *)

val block_speed : float
(** [block_speed] is the speed at which blocks move. *)

val initial_block_factor : float
(** [initial_block_factor] is the initial scaling factor for newly emerging
    blocks. *)

val block_scaling : float
(** [block_scaling] is the scaling rate of blocks during animations. *)

val block_position_mapping : int * int -> float * float
(** [block_position_mapping] converts grid coordinates to pixel positions. *)

val block_board_mapping : float * float -> int * int
(** [block_board_mapping] converts pixel positions back to grid coordinates. *)
