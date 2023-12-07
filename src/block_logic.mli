open Block
(** This file handles block movement, random block generation, and block merging
    logic *)

type block = Block.block

val compress : block list -> block list
(** Removes 0's within an block list and returns the block compressed list
    without blokcks with value 0. ex. compress [3; 3; 0; 3] -> [3; 3; 3]*)

val correct_pos_state_lateral : int -> block list -> unit
(** Takes in the current row number and the block list represeting the row and
    corrests the block states according to its position *)

val correct_pos_state_vertical : block list list -> unit
(** Takes in the current board and iterates through each column and row,
    correcting the blocks states according to its position*)

val l_merge : block list -> block list * int
(** Combines equal numbers and shifts numbers to the left. Numbers on the
    left-most side receive priority. ex. l_merge [2; 2; 0; 2] -> [4; 2] *)

val r_merge : block list -> block list * int
(** Combines equal numbers and shifts numbers to the right. Numbers on the
    right-most side receive priority. ex. l_merge [2; 2; 0; 2] -> [2; 4] *)

val l_move : int -> block list -> block list * int
(** Uses l_merge and compress to shift a row to the left to simulate a left
    button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
    preserve the number of squares in a list. ex. l_move [2; 2; 0; 2] ->
    [4; 2;
   0; 0]*)

val r_move : int -> block list -> block list * int
(** Uses r_merge and compress to shift a row to the right to simulate a right
    button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
    preserve the number of squares in a list. ex. r_move [2; 2; 0; 2] ->
    [0; 0;
   2; 4]*)

val transpose : 'a list list -> 'a list list
(** Transposes a matrix ('a list list) so that the columns become the rows. ex.
    transpose [[1;2;3;4];[5;6;7;8];[9;10;11;12];[13;14;15;16]] ->
    [[1;5;9;13];[2;6;10;14];[3;7;11;15];[4;8;12;16]] *)

val up_move_aux : block list -> block list * int
(** Similar to l_move. Only difference is correct_pos_state_laterial is not
    called *)

val u_move : block list list -> block list list * int
(** Uses transpose and horizontal move functions to simulate an up button press
    in 2048. Populates rows w/ < num_squares entries w/ 0s to preserve the
    number of squares in a list. ex. u_move
    [[2;0;0;0];[2;0;0;0];[0;0;0;0];[0;0;0;0]] ->
    [[4;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]*)

val down_move_aux : block list -> block list * int
(** Similar to r_move. Only difference is correct_pos_state_laterial is not
    called *)

val d_move : block list list -> block list list * int
(** Uses transpose and horizontal move functions to simulate an down button
    press in 2048. Populates rows w/ < num_squares entries w/ 0s to preserve the
    number of squares in a list. ex. u_move
    [[2;0;0;0];[2;0;0;0];[0;0;0;0];[0;0;0;0]] ->
    [[0;0;0;0];[0;0;0;0];[0;0;0;0];[4;0;0;0]]*)

val calculate_next : block list list -> int -> block list list * int
(** Shifts the 4x4 board left, right, up, or down depending on the input
    parameter. The board is expected as an int list list and the function
    returns the new board. ex. calculate_next
    [[2; 2; 0; 0]; [0; 0; 0; 0]; [4; 4; 8; 0];
   [0; 0; 2; 0]] move_left ->
    [[4; 0; 0; 0]; [0; 0; 0; 0]; [8; 8; 0; 0]; [2; 0;
   0; 0]]*)

val random_mag : unit -> int
(** Determines the magnitude of the randomly generated block by returning 4 or 2*)

val find_zeros : block list list -> (int * int) list
(** Finds the locations of the empty blocks (0's) within the board *)

val count_empty : block list list -> int
(** Returns the number of empty blocks within the board *)

val generate_block : block list list -> block list list
(** Generates a random block within the board in a random location with a
    magnitude of either 4 or 2*)

val generate_initial : unit -> block list list
(** Generate an initial board that has two blocks (either 4 or 2) in a random
    location on the board*)
