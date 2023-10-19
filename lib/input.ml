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
  | a :: b :: t when a = b ->
      let merged_list, score = l_merge t in
      ((a + b) :: merged_list, a + b + score)
  | a :: t ->
      let merged_list, score = l_merge t in
      (a :: merged_list, score)
  | [] -> ([], 0)

(* Uses l_merge and l_compress to shift a row to the left to simulate a left
   button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
   preserve the number of squares in a list. ex. l_move [2; 2; 0; 2] -> [4; 2;
   0; 0]*)
let l_move row =
  let compressed = l_compress row in
  let merged, score = l_merge compressed in
  let result = l_compress merged in
  (result @ List.init (List.length row - List.length result) (fun _ -> 0), score)

(* Uses l_merge and l_compress to shift a row to the right to simulate a right
   button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
   preserve the number of squares in a list. ex. r_move [2; 2; 0; 2] -> [0; 0;
   2; 4]*)
let r_move row =
  let rev = List.rev row in
  let result, score = l_move rev in
  ( List.rev
      (result @ List.init (List.length row - List.length result) (fun _ -> 0)),
    score )

(* Shifts the 4x4 board left, right, up, or down depending on the input
   parameter. The board is expected as an int list list and the function returns
   the new board. ex. calculate_next [[2; 2; 0; 0]; [0; 0; 0; 0]; [4; 4; 8; 0];
   [0; 0; 2; 0]] move_left -> [[4; 0; 0; 0]; [0; 0; 0; 0]; [8; 8; 0; 0]; [2; 0;
   0; 0]]*)
let calculate_next board dir =
  match dir with
  | dir when dir = move_left ->
      let moved_board, scores = List.split (List.map l_move board) in
      (moved_board, List.fold_left ( + ) 0 scores)
  | dir when dir = move_right ->
      let moved_board, scores = List.split (List.map r_move board) in
      (moved_board, List.fold_left ( + ) 0 scores)
  | _ -> failwith "TODO"

(* else if dir = move_down then List.map d_move board else if dir = move_up then
   List.map u_move board *)
