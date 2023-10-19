open Constants

(* Helper function that removes 0's within a list and returns the compressed
   list ex. l_compress [3; 3; 0; 3] -> [3; 3; 3]*)
let rec l_compress = function
  | [] -> []
  | 0 :: t -> l_compress t
  | h :: t -> h :: l_compress t

(* Helper function that combines equal numbers and shifts numbers to the left.
   Numbers on the left-most side receive priority. ex. l_merge [2; 2; 0; 2] ->
   [4; 2] *)
let rec l_merge = function
  | a :: b :: t when a = b -> (a + b) :: l_merge t
  | a :: t -> a :: l_merge t
  | [] -> []

(* Uses l_merge and l_compress to shift a row to the left to simulate a left
   button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
   preserve the number of squares in a list. ex. l_move [2; 2; 0; 2] -> [4; 2;
   0; 0]*)
let l_move row =
  let compressed = l_compress row in
  let merged = l_merge compressed in
  let result = l_compress merged in
  result @ List.init (List.length row - List.length result) (fun _ -> 0)

(* Uses l_merge and l_compress to shift a row to the right to simulate a right
   button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
   preserve the number of squares in a list. ex. r_move [2; 2; 0; 2] -> [0; 0;
   2; 4]*)
let r_move row =
  let rev = List.rev row in
  let result = l_move rev in
  List.rev
    (result @ List.init (List.length row - List.length result) (fun _ -> 0))

(* Shifts the 4x4 board left, right, up, or down depending on the input
   parameter. The board is expected as an int list list and the function returns
   the new board. ex. calculate_next [[2; 2; 0; 0]; [0; 0; 0; 0]; [4; 4; 8; 0];
   [0; 0; 2; 0]] move_left -> [[4; 0; 0; 0]; [0; 0; 0; 0]; [8; 8; 0; 0]; [2; 0;
   0; 0]]*)
let calculate_next board dir =
  if dir = move_left then List.map l_move board
  else if dir = move_right then List.map r_move board
  else failwith "TODO"
(* else if dir = move_down then List.map d_move board else if dir = move_up then
   List.map u_move board *)
