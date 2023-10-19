open Constants
open Random

(* Block movement *)

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
let l_move (row : int list) : int list * int =
  let compressed = l_compress row in
  let merged, score = l_merge compressed in
  let result = l_compress merged in
  (result @ List.init (List.length row - List.length result) (fun _ -> 0), score)

(* Uses l_merge and l_compress to shift a row to the right to simulate a right
   button press in 2048. Populates rows w/ < num_squares entries w/ 0s to
   preserve the number of squares in a list. ex. r_move [2; 2; 0; 2] -> [0; 0;
   2; 4]*)
let r_move (row : int list) : int list * int =
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
let calculate_next (board : int list list) (dir : int) : int list list * int =
  match dir with
  | dir when dir = move_left ->
      let moved_board, scores = List.split (List.map l_move board) in
      (moved_board, List.fold_left ( + ) 0 scores)
  | dir when dir = move_right ->
      let moved_board, scores = List.split (List.map r_move board) in
      (moved_board, List.fold_left ( + ) 0 scores)
  | _ -> failwith "TODO"

(*****************************************************************************)
(* Random block generation *)

let random_mag () =
  let rand = Random.int 10 in
  match rand with
  | _ when rand = 5 -> 4
  | _ -> 2

let find_zeros (board : int list list) : (int * int list) list =
  List.mapi
    (fun ind_1 a ->
      (ind_1, List.mapi (fun ind_2 b -> if b = 0 then ind_2 else -1) a))
    board

let count_empty (lst : (int * int list) list) : int =
  List.fold_left
    (fun acc (_, int_list) ->
      acc + List.length (List.filter (fun x -> x >= 0) int_list))
    0 lst

let generate_block board =
  let mag = random_mag () in
  let zero_lst = find_zeros board in
  let loc = Random.int (count_empty zero_lst) in

  let rec find_nth_empty n lst =
    match lst with
    | [] -> failwith "Error: Empty list"
    | (row, cols) :: t ->
        let valid_cols = List.filter (fun x -> x >= 0) cols in
        if n < List.length valid_cols then (row, List.nth valid_cols n)
        else find_nth_empty (n - List.length valid_cols) t
  in

  let target_row, target_col = find_nth_empty loc zero_lst in

  List.mapi
    (fun i row ->
      if i = target_row then
        List.mapi (fun j cell -> if j = target_col then mag else cell) row
      else row)
    board
