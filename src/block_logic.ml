open Constants
open Block
open Random

(* Block movement *)
type block = Block.block

let rec compress = function
  | [] -> []
  | h :: t when Block.get_value h = 0 -> compress t
  | h :: t -> h :: compress t

let correct_pos_state_lateral (row_n : int) (row : block list) : unit =
  for i = 0 to num_squares - 1 do
    let cur_block = List.nth row i in
    Block.set_target_grid_pos cur_block i row_n;
    let cur_pos = Block.get_current_actual_pos cur_block in
    let target_pos = Block.get_target_actual_pos cur_block in
    if
      fst cur_pos != fst target_pos
      || (snd cur_pos != snd target_pos && Block.get_state cur_block != Blank)
    then Block.set_state cur_block (Moving 0.)
    else Block.set_state cur_block Stationary
  done

let correct_pos_state_vertical (board : block list list) : unit =
  for i = 0 to num_squares - 1 do
    for j = 0 to num_squares - 1 do
      let cur_block = List.nth (List.nth board i) j in
      Block.set_target_grid_pos cur_block j i;
      let cur_pos = Block.get_current_actual_pos cur_block in
      let target_pos = Block.get_target_actual_pos cur_block in
      if
        (fst cur_pos != fst target_pos || snd cur_pos != snd target_pos)
        && Block.get_state cur_block != Blank
      then Block.set_state cur_block (Moving 0.)
      else Block.set_state cur_block Stationary
    done
  done

let rec l_merge = function
  | a :: b :: t when Block.get_value a = Block.get_value b ->
      let merged_list, score = l_merge t in
      let merged_block =
        Block.set_value a (Block.get_value a + Block.get_value b);
        Block.set_state a Merging;
        a
      in
      ( merged_block :: merged_list,
        Block.get_value a + Block.get_value b + score )
  | a :: t ->
      let merged_list, score = l_merge t in
      (a :: merged_list, score)
  | [] -> ([], 0)

let r_merge lst =
  let reversed_lst = List.rev lst in
  let merged, score = l_merge reversed_lst in
  (List.rev merged, score)

let l_move (row_n : int) (row : block list) : block list * int =
  let compressed = compress row in
  let merged, score = l_merge compressed in
  let result = compress merged in
  let padding =
    List.init
      (List.length row - List.length result)
      (fun n -> Block.place_blank_block 0 (n + List.length result, row_n))
  in
  correct_pos_state_lateral row_n (result @ padding);
  (result @ padding, score)

let r_move (row_n : int) (row : block list) : block list * int =
  let compressed = compress row in
  let merged, score = r_merge compressed in
  let result_length = List.length merged in
  let padding =
    List.init
      (List.length row - result_length)
      (fun n -> Block.place_blank_block 0 (n, row_n))
  in
  correct_pos_state_lateral row_n (padding @ merged);
  (padding @ merged, score)

let transpose matrix =
  match matrix with
  | [] -> []
  | [] :: _ -> []
  | _ ->
      let rec innerTranspose mat acc =
        if List.exists (fun x -> x <> []) mat then
          innerTranspose
            (List.map
               (function
                 | [] -> []
                 | h :: t -> t)
               mat)
            (List.map
               (function
                 | [] -> failwith "Invalid matrix"
                 | h :: t -> h)
               mat
            :: acc)
        else List.rev acc
      in
      innerTranspose matrix []

let up_move_aux (row : block list) =
  let compressed = compress row in
  let merged, score = l_merge compressed in
  let result = compress merged in
  let padding =
    List.init
      (List.length row - List.length result)
      (fun n -> Block.place_blank_block 0 (0, 0))
  in
  (result @ padding, score)

let u_move (board : block list list) : block list list * int =
  let transposed_board = transpose board in
  let moved_board, scores =
    List.split (List.map up_move_aux transposed_board)
  in
  let final_board = transpose moved_board in
  correct_pos_state_vertical final_board;
  (final_board, List.fold_left ( + ) 0 scores)

let down_move_aux (row : block list) =
  let compressed = compress row in
  let merged, score = r_merge compressed in
  let result_length = List.length merged in
  let padding =
    List.init
      (List.length row - result_length)
      (fun n -> Block.place_blank_block 0 (0, 0))
  in
  (padding @ merged, score)

let d_move (board : block list list) : block list list * int =
  let transposed_board = transpose board in
  let moved_board, scores =
    List.split (List.map down_move_aux transposed_board)
  in
  let final_board = transpose moved_board in
  correct_pos_state_vertical final_board;
  (final_board, List.fold_left ( + ) 0 scores)

(* Shifts the 4x4 board left, right, up, or down depending on the input
   parameter. The board is expected as an int list list and the function returns
   the new board. ex. calculate_next [[2; 2; 0; 0]; [0; 0; 0; 0]; [4; 4; 8; 0];
   [0; 0; 2; 0]] move_left -> [[4; 0; 0; 0]; [0; 0; 0; 0]; [8; 8; 0; 0]; [2; 0;
   0; 0]]*)
   let calculate_next (board : block list list) (dir : int) : block list list * int =
    match dir with
    | dir when dir = move_left ->
        let moved_board, scores = List.split (List.mapi l_move board) in
        (moved_board, List.fold_left ( + ) 0 scores)
    | dir when dir = move_right ->
        let moved_board, scores = List.split (List.mapi r_move board) in
        (moved_board, List.fold_left ( + ) 0 scores)
    | dir when dir = move_up ->
        let moved_board, scores = u_move board in
        (moved_board, scores)
    | dir when dir = move_down ->
        let moved_board, scores = d_move board in
        (moved_board, scores)
    | _ -> failwith "Invalid direction"
  (* *)

(* Random block generation *)
let random_mag () =
  let rand = Random.int 10 in
  match rand with
  | _ when rand = 5 -> 4
  | _ -> 2

let find_zeros (board : block list list) : (int * int) list =
  List.mapi
    (fun row_index row ->
      List.fold_left
        (fun acc (col_index, block) ->
          if Block.get_value block = 0 then (row_index, col_index) :: acc
          else acc)
        []
        (List.mapi (fun col_index block -> (col_index, block)) row))
    board
  |> List.flatten

let count_empty (board : block list list) : int =
  List.fold_left
    (fun acc row ->
      acc
      + List.fold_left
          (fun acc block -> if Block.get_value block = 0 then acc + 1 else acc)
          0 row)
    0 board

let generate_block board =
  Random.self_init ();
  let mag = random_mag () in
  let zero_lst = find_zeros board in
  let loc = Random.int (count_empty board) in
  let find_nth_empty n lst =
    if n < List.length lst then List.nth lst n
    else failwith "Error: Index out of bounds"
  in
  let target_row, target_col = find_nth_empty loc zero_lst in
  let new_board =
    List.mapi
      (fun i row ->
        if i = target_row then
          List.mapi
            (fun j cell ->
              if j = target_col then (
                let block = Block.place_block mag (j, i) in
                Block.set_target_grid_pos block j i;
                Block.set_state block (Emerging 0.);
                block)
              else cell)
            row
        else row)
      board
  in
  new_board

let generate_initial () =
  Random.self_init ();
  let random_mag () = if Random.float 1. < 0.9 then 2 else 4 in
  let rec set_block board col row value =
    List.mapi
      (fun i r ->
        if i = row then
          List.mapi
            (fun j c -> if j = col then Block.place_block value (j, i) else c)
            r
        else r)
      board
  in
  let rec get_random_empty_position board =
    let row = Random.int 4 in
    let col = Random.int 4 in
    let block = List.nth (List.nth board row) col in
    if Block.get_value block = 0 then (col, row)
    else get_random_empty_position board
  in

  let board = Block.empty_board in
  let row1, col1 = get_random_empty_position board in
  let board = set_block board row1 col1 (random_mag ()) in
  let row2, col2 = get_random_empty_position board in
  let board = set_block board row2 col2 (random_mag ()) in
  board
