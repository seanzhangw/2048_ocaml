open OUnit2
open G2048
open Block_logic
open Constants
open Block

(* Test Plan: *)
(* Approach: Since how we are presenting our game is in a graphical format, it
   is hard for us to test the graphics in this suite. Thus, this test suite only
   tests the functions that handle the data (Block_logic module) after
   interacting with the player and the displaying of information is only
   manually tested. The test cases tested in this suite is a mix of black-box,
   glass-box, and randomized testing. Each test case is titled with what it is
   used to test for. In this file, we included pretty printing to help us
   ifentify the issues with our code if the test cases were to fail. We
   organized the test cases by grouping them into each specific function. We
   believe that the tests included in this suite demonstrates the correctness of
   the system as it tests all of the functions involved in the interaction with
   the data type used to store the game information. *)

let pp_pair pp_l pp_r (l, r) = Printf.sprintf "(%s, %s)" (pp_l l) (pp_r r)

(* let pp_list pp_elt lst = let pp_elts lst = let rec loop n acc = function | []
   -> acc | [ h ] -> acc ^ pp_elt h | h1 :: (h2 :: t as t') -> if n = 100 then
   acc ^ "..." else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t' in loop 0 "" lst in
   Printf.sprintf "[%s]" (pp_elts lst)

   let pp_int_list = pp_list string_of_int *)
(* let pp_int_list_int_pair = pp_pair pp_int_list string_of_int *)

(* let pp_int_matrix matrix = let pp_row row = "[" ^ String.concat "; "
   (List.map string_of_int row) ^ "]" in "[" ^ String.concat "; " (List.map
   pp_row matrix) ^ "]"

   let pp_matrix_int_pair (matrix, score) = Printf.sprintf "(%s, %d)"
   (pp_int_matrix matrix) score *)

(* Pretty Printer for block *)
let pp_block_list blocks =
  "[" ^ String.concat "; " (List.map Block.to_string blocks) ^ "]"

(* Function to convert a list of integers to a list of blocks *)
let to_block_list int_list =
  List.map (fun v -> Block.place_block v (0, 0)) int_list

(* Pretty printer for a pair consisting of a block list and an integer score *)
let to_block_matrix int_matrix =
  List.mapi
    (fun row_index row ->
      List.mapi
        (fun col_index value -> Block.place_block value (col_index, row_index))
        row)
    int_matrix

(* Pretty printer for a pair consisting of a block list and an integer score *)
let pp_block_list_int_pair (block_list, score) =
  Printf.sprintf "(%s, %s)" (pp_block_list block_list) (string_of_int score)

(* Pretty printer for a block matrix *)
let pp_block_matrix matrix =
  let pp_row row = pp_block_list row in
  "[" ^ String.concat "; " (List.map pp_row matrix) ^ "]"

(* Pretty printer for a pair consisting of a block matrix and an integer
   score *)
let pp_block_matrix_int_pair (matrix, score) =
  Printf.sprintf "(%s, %s)" (pp_block_matrix matrix) (string_of_int score)

(* test functions *)

(* Test function for the compress funtion *)
let compress_test out in1 _ =
  assert_equal ~printer:pp_block_list out (compress in1)

(* Test function for the l_merge funtion *)
let l_merge_test expected input _ =
  let expected_str = pp_pair pp_block_list string_of_int expected in
  let actual_str = pp_pair pp_block_list string_of_int (l_merge input) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: l_merge\ninput: " ^ pp_block_list input ^ "\nexpected: "
     ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

(* Test function for the r_merge funtion *)
let r_merge_test expected input _ =
  let expected_str = pp_pair pp_block_list string_of_int expected in
  let actual_str = pp_pair pp_block_list string_of_int (r_merge input) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: r_merge\ninput: " ^ pp_block_list input ^ "\nexpected: "
     ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

(* Test function for the l_move funtion *)
let l_move_test expected row_n input _ =
  let expected_str = pp_block_list_int_pair expected in
  let actual_str = pp_block_list_int_pair (l_move row_n input) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: l_move\ninput: " ^ pp_block_list input ^ "\nexpected: "
     ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

(* Test function for the r_move funtion *)
let r_move_test expected row_n input _ =
  let expected_str = pp_block_list_int_pair expected in
  let actual_str = pp_block_list_int_pair (r_move row_n input) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: r_move\ninput: " ^ pp_block_list input ^ "\nexpected: "
     ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

(* Test function for the transpose funtion *)
let transpose_test expected input _ =
  let expected_str = pp_block_matrix expected in
  let actual_str = pp_block_matrix (transpose input) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: transpose\ninput: " ^ pp_block_matrix input ^ "\nexpected: "
     ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

(* Test function for the u_move funtion *)
let u_move_test out in1 _ =
  assert_equal ~printer:pp_block_matrix_int_pair out (u_move in1)

(* Test function for the d_move funtion *)
let d_move_test out in1 _ =
  assert_equal ~printer:pp_block_matrix_int_pair out (d_move in1)

(* Test function for the calculate_next funtion *)

let calculate_next_test expected board dir _ =
  let expected_str = pp_block_matrix_int_pair expected in
  let actual_str = pp_block_matrix_int_pair (calculate_next board dir) in
  assert_equal
    ~printer:(fun x -> x)
    ~msg:
      ("function: calculate_next\ninput1: " ^ pp_block_matrix board
     ^ "\nexpected: " ^ expected_str ^ "\nactual: " ^ actual_str)
    expected_str actual_str

let single_one_list = [ 1 ]
let ones_list = [ 1; 1; 1; 1 ]
let one_to_four_list = [ 1; 2; 3; 4 ]
let one_to_three_list = [ 1; 2; 3 ]
let one_two_list = [ 1; 2 ]
let all_zeros = [ 0; 0; 0; 0 ]
let single_zero = [ 0 ]
let spread_zeros = [ 2; 0; 1; 0 ]
let alternating_zeros = [ 0; 1; 0; 2; 0; 3; 0 ]
let all_zeros_except_one = [ 0; 0; 1; 0; 0 ]
let zeros_at_start = [ 0; 0; 1; 2 ]
let zeros_at_end = [ 1; 2; 0; 0 ]
let zero_on_left = [ 0; 1; 2; 3 ]
let zero_on_right = [ 1; 2; 3; 0 ]
let alternate_zeros_input = [ 0; 2; 0; 4; 0 ]
let alternate_zeros_output = [ 2; 4 ]

(* Test cases testing compress *)
let compress_tests =
  [
    "Testing all 0" >:: compress_test [] [];
    "Testing all 0" >:: compress_test [] (to_block_list all_zeros);
    "Testing single zero" >:: compress_test [] (to_block_list single_zero);
    "Testing single non-zero"
    >:: compress_test
          (to_block_list single_one_list)
          (to_block_list single_one_list);
    "Testing all non-empty"
    >:: compress_test (to_block_list ones_list) (to_block_list ones_list);
    "Testing non-zero elements only"
    >:: compress_test
          (to_block_list one_to_four_list)
          (to_block_list one_to_four_list);
    "Testing 0 spreaded out"
    >:: compress_test
          (List.rev (to_block_list one_two_list))
          (to_block_list spread_zeros);
    "Testing alternating zeros and\n\n   non-zeros"
    >:: compress_test
          (to_block_list one_to_three_list)
          (to_block_list alternating_zeros);
    "Testing\n\n   all zeros except one"
    >:: compress_test
          (to_block_list single_one_list)
          (to_block_list all_zeros_except_one);
    "Testing zeros at start"
    >:: compress_test
          (to_block_list one_two_list)
          (to_block_list zeros_at_start);
    "Testing zeros at end"
    >:: compress_test (to_block_list one_two_list) (to_block_list zeros_at_end);
    "Testing\n 0 only on the left"
    >:: compress_test
          (to_block_list one_to_three_list)
          (to_block_list zero_on_left);
    "Testing 0 only on the right"
    >:: compress_test
          (to_block_list one_to_three_list)
          (to_block_list zero_on_right);
    "Testing alternating zeros"
    >:: compress_test
          (to_block_list alternate_zeros_output)
          (to_block_list alternate_zeros_input);
  ]

let list_of_zeros = [ 0; 0; 0; 0 ]
let list_of_two_zeros = [ 0; 0 ]
let basic_merge_left_input = [ 2; 2; 2; 0 ]
let basic_merge_left_output = [ 4; 2; 0 ]
let no_merge_left_input = [ 2; 4; 2; 4 ]
let no_merge_left_output = [ 2; 4; 2; 4 ]
let multiple_merge_left_input = [ 2; 2; 4; 4 ]
let multiple_merge_left_output = [ 4; 8 ]
let merge_leading_zero_left_input = [ 0; 2; 2; 8 ]
let merge_leading_zero_left_output = [ 0; 4; 8 ]
let basic_merge_right_input = [ 2; 2; 2; 0 ]
let basic_merge_right_output = [ 2; 4; 0 ]
let no_merge_right_input = [ 2; 4; 2; 4 ]
let no_merge_right_output = [ 2; 4; 2; 4 ]
let multiple_merge_right_input = [ 4; 4; 2; 2 ]
let multiple_merge_right_output = [ 8; 4 ]
let merge_trailing_zero_right_input = [ 8; 2; 2; 0 ]
let merge_trailing_zero_right_output = [ 8; 4; 0 ]
let l_merge_leading_zeros_input = [ 0; 0; 2; 2 ]
let l_merge_leading_zeros_output = [ 0; 4 ]
let r_merge_leading_zeros_input = [ 2; 2; 0; 0 ]
let r_merge_leading_zeros_output = [ 4; 0 ]

(* Test cases testing the merge functions *)
let merge_tests =
  [
    "Testing list of 0s lists with left merge"
    >:: l_merge_test
          (to_block_list list_of_two_zeros, 0)
          (to_block_list list_of_zeros);
    "Basic merge with left\n merge"
    >:: l_merge_test
          (to_block_list basic_merge_left_output, 4)
          (to_block_list basic_merge_left_input);
    "No merge with left merge"
    >:: l_merge_test
          (to_block_list no_merge_left_output, 0)
          (to_block_list no_merge_left_input);
    "Multiple merges with left merge"
    >:: l_merge_test
          (to_block_list multiple_merge_left_output, 12)
          (to_block_list multiple_merge_left_input);
    "Merge with leading zero in left merge"
    >:: l_merge_test
          (to_block_list merge_leading_zero_left_output, 4)
          (to_block_list merge_leading_zero_left_input);
    "Testing list of 0s with right merge"
    >:: r_merge_test
          (to_block_list list_of_two_zeros, 0)
          (to_block_list list_of_zeros);
    "Basic merge with right merge"
    >:: r_merge_test
          (to_block_list basic_merge_right_output, 4)
          (to_block_list basic_merge_right_input);
    "No merge with right merge"
    >:: r_merge_test
          (to_block_list no_merge_right_output, 0)
          (to_block_list no_merge_right_input);
    "Multiple merges with right\n merge"
    >:: r_merge_test
          (to_block_list multiple_merge_right_output, 12)
          (to_block_list multiple_merge_right_input);
    "Merge with trailing zero in right merge"
    >:: r_merge_test
          (to_block_list merge_trailing_zero_right_output, 4)
          (to_block_list merge_trailing_zero_right_input);
    "Merge with leading zeros in left merge"
    >:: l_merge_test
          (to_block_list l_merge_leading_zeros_output, 4)
          (to_block_list l_merge_leading_zeros_input);
    "Merge with leading zeros in right merge"
    >:: l_merge_test
          (to_block_list r_merge_leading_zeros_output, 4)
          (to_block_list r_merge_leading_zeros_input);
  ]

let left_move_input = [ 2; 2; 0; 2 ]
let left_move_output = [ 4; 2; 0; 0 ]
let left_move_with_merge_input = [ 2; 2; 0; 0 ]
let left_move_with_merge_output = [ 4; 0; 0; 0 ]
let left_move_no_merge_input = [ 2; 0; 4; 2 ]
let left_move_no_merge_output = [ 2; 4; 2; 0 ]
let left_move_complex_merge_input = [ 2; 2; 2; 2 ]
let left_move_complex_merge_output = [ 4; 4; 0; 0 ]
let left_move_single_nonzero_input = [ 2; 0; 0; 0 ]
let right_move_input = [ 2; 2; 0; 2 ]
let right_move_output = [ 0; 0; 2; 4 ]
let right_move_with_merge_input = [ 2; 2; 0; 0 ]
let right_move_with_merge_output = [ 0; 0; 0; 4 ]
let right_move_no_merge_input = [ 2; 0; 4; 2 ]
let right_move_no_merge_output = [ 0; 2; 4; 2 ]
let right_move_complex_merge_input = [ 2; 2; 2; 2 ]
let right_move_complex_merge_output = [ 0; 0; 4; 4 ]
let right_move_single_nonzero_input = [ 0; 0; 0; 2 ]
let only_zero = [ 0; 0; 0; 0 ]

(* Test cases testing the move functions *)
let move_tests =
  [
    "Testing left move"
    >:: l_move_test
          (to_block_list left_move_output, 4)
          0
          (to_block_list left_move_input);
    "Left move with merge and trailing zeros"
    >:: l_move_test
          (to_block_list left_move_with_merge_output, 4)
          0
          (to_block_list left_move_with_merge_input);
    "Left move with no merge"
    >:: l_move_test
          (to_block_list left_move_no_merge_output, 0)
          0
          (to_block_list left_move_no_merge_input);
    "Left move with complex merge"
    >:: l_move_test
          (to_block_list left_move_complex_merge_output, 8)
          0
          (to_block_list left_move_complex_merge_input);
    "Left move with single non-zero"
    >:: l_move_test
          (to_block_list left_move_single_nonzero_input, 0)
          0
          (to_block_list left_move_single_nonzero_input);
    "Left move with only zero"
    >:: l_move_test (to_block_list only_zero, 0) 0 (to_block_list only_zero);
    "Testing\n right move"
    >:: r_move_test
          (to_block_list right_move_output, 4)
          0
          (to_block_list right_move_input);
    "Right\n move with merge and leading zeros"
    >:: r_move_test
          (to_block_list right_move_with_merge_output, 4)
          0
          (to_block_list right_move_with_merge_input);
    "Right move\n with no merge"
    >:: r_move_test
          (to_block_list right_move_no_merge_output, 0)
          0
          (to_block_list right_move_no_merge_input);
    "Right move with complex merge"
    >:: r_move_test
          (to_block_list right_move_complex_merge_output, 8)
          0
          (to_block_list right_move_complex_merge_input);
    "Right move with single non-zero"
    >:: r_move_test
          (to_block_list right_move_single_nonzero_input, 0)
          0
          (to_block_list right_move_single_nonzero_input);
    "Right move with only zero"
    >:: r_move_test (to_block_list only_zero, 0) 0 (to_block_list only_zero);
  ]

let identity_matrix =
  [ [ 1; 0; 0; 0 ]; [ 0; 1; 0; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 0; 1 ] ]

let zero_matrix =
  [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]

let matrix1 =
  [ [ 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ]; [ 9; 10; 11; 12 ]; [ 13; 14; 15; 16 ] ]

let matrix1t =
  [ [ 1; 5; 9; 13 ]; [ 2; 6; 10; 14 ]; [ 3; 7; 11; 15 ]; [ 4; 8; 12; 16 ] ]

let matrix2 = [ [ 1; 0; 1; 0 ]; [ 0; 1; 0; 1 ]; [ 1; 0; 1; 0 ]; [ 0; 1; 0; 1 ] ]

let matrix2t =
  [ [ 1; 0; 1; 0 ]; [ 0; 1; 0; 1 ]; [ 1; 0; 1; 0 ]; [ 0; 1; 0; 1 ] ]

let single_row_matrix = [ [ 1; 2; 3; 4 ] ]
let single_row_matrix_t = [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]
let single_column_matrix = [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ] ]
let single_column_matrix_t = [ [ 1; 2; 3; 4 ] ]
let large_matrix = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ]; [ 10; 11; 12 ] ]
let large_matrix_t = [ [ 1; 4; 7; 10 ]; [ 2; 5; 8; 11 ]; [ 3; 6; 9; 12 ] ]
let unequal_row_col_matrix = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]
let unequal_row_col_matrix_t = [ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ]
let negative_values_matrix = [ [ -1; -2; -3 ]; [ -4; -5; -6 ]; [ -7; -8; -9 ] ]

let negative_values_matrix_t =
  [ [ -1; -4; -7 ]; [ -2; -5; -8 ]; [ -3; -6; -9 ] ]

let single_element_matrix = [ [ 1 ] ]
let random_values_nonsquare = [ [ 9; 2; 5 ]; [ 1; 7; 4 ]; [ 3; 6; 8 ] ]
let random_values_nonsquare_t = [ [ 9; 1; 3 ]; [ 2; 7; 6 ]; [ 5; 4; 8 ] ]

(* Test cases testing the transpose function *)
let transpose_tests =
  [
    "Testing transpose on identity matrix"
    >:: transpose_test
          (to_block_matrix identity_matrix)
          (to_block_matrix identity_matrix);
    "Testing transpose on zero\n matrix"
    >:: transpose_test
          (to_block_matrix zero_matrix)
          (to_block_matrix zero_matrix);
    "Testing\n   transpose on\n non-empty matrix"
    >:: transpose_test (to_block_matrix matrix1) (to_block_matrix matrix1t);
    "Testing transpose on\n non-empty matrix with\n   zeros"
    >:: transpose_test (to_block_matrix matrix2) (to_block_matrix matrix2t);
    "Testing\n transpose of a single row matrix"
    >:: transpose_test
          (to_block_matrix single_row_matrix)
          (to_block_matrix single_row_matrix_t);
    "Testing transpose of a single column matrix"
    >:: transpose_test
          (to_block_matrix single_column_matrix)
          (to_block_matrix single_column_matrix_t);
    "Testing\n transpose of a large matrix"
    >:: transpose_test
          (to_block_matrix large_matrix)
          (to_block_matrix large_matrix_t);
    "Testing transpose of an unequal row and column matrix"
    >:: transpose_test
          (to_block_matrix unequal_row_col_matrix)
          (to_block_matrix unequal_row_col_matrix_t);
    "Testing transpose of a\n matrix with negative\n   values"
    >:: transpose_test
          (to_block_matrix negative_values_matrix)
          (to_block_matrix negative_values_matrix_t);
    "Transpose of a single-element matrix"
    >:: transpose_test
          (to_block_matrix single_element_matrix)
          (to_block_matrix single_element_matrix);
    "Transpose of a random value non-square matrix"
    >:: transpose_test
          (to_block_matrix random_values_nonsquare)
          (to_block_matrix random_values_nonsquare_t);
  ]

let identity_matrix_r =
  [ [ 0; 0; 0; 1 ]; [ 0; 0; 0; 1 ]; [ 0; 0; 0; 1 ]; [ 0; 0; 0; 1 ] ]

let identity_matrix_l =
  [ [ 1; 0; 0; 0 ]; [ 1; 0; 0; 0 ]; [ 1; 0; 0; 0 ]; [ 1; 0; 0; 0 ] ]

let identity_matrix_u =
  [ [ 1; 1; 1; 1 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]

let identity_matrix_d =
  [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 1; 1; 1; 1 ] ]

let alternate_values_matrix =
  [ [ 2; 0; 2; 0 ]; [ 0; 4; 0; 4 ]; [ 2; 0; 2; 0 ]; [ 0; 4; 0; 4 ] ]

let alternate_values_matrix_r =
  [ [ 0; 0; 0; 4 ]; [ 0; 0; 0; 8 ]; [ 0; 0; 0; 4 ]; [ 0; 0; 0; 8 ] ]

let alternate_values_matrix_l =
  [ [ 4; 0; 0; 0 ]; [ 8; 0; 0; 0 ]; [ 4; 0; 0; 0 ]; [ 8; 0; 0; 0 ] ]

let alternate_values_matrix_u =
  [ [ 4; 8; 4; 8 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]

let alternate_values_matrix_d =
  [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 4; 8; 4; 8 ] ]

let no_movement_matrix =
  [ [ 2; 4; 8; 16 ]; [ 16; 8; 4; 2 ]; [ 2; 4; 8; 16 ]; [ 16; 8; 4; 2 ] ]

(* test cases testing the calculate_next function *)
let calculate_next_tests =
  [
    "Testing calculate next on empty matrix with\n   right"
    >:: calculate_next_test
          (to_block_matrix zero_matrix, 0)
          (to_block_matrix zero_matrix)
          move_right;
    "Testing calculate next on empty matrix with left"
    >:: calculate_next_test
          (to_block_matrix zero_matrix, 0)
          (to_block_matrix zero_matrix)
          move_left;
    "Testing calculate next on empty\n   matrix with up"
    >:: calculate_next_test
          (to_block_matrix zero_matrix, 0)
          (to_block_matrix zero_matrix)
          move_up;
    "Testing calculate next on empty matrix with down"
    >:: calculate_next_test
          (to_block_matrix zero_matrix, 0)
          (to_block_matrix zero_matrix)
          move_down;
    "Testing calculate next on empty\n   matrix with right"
    >:: calculate_next_test
          (to_block_matrix identity_matrix_r, 0)
          (to_block_matrix identity_matrix)
          move_right;
    "Testing calculate next on empty matrix with\n   left"
    >:: calculate_next_test
          (to_block_matrix identity_matrix_l, 0)
          (to_block_matrix identity_matrix)
          move_left;
    "Testing calculate next on empty matrix with up"
    >:: calculate_next_test
          (to_block_matrix identity_matrix_u, 0)
          (to_block_matrix identity_matrix)
          move_up;
    "Testing\n   calculate next on empty matrix with down"
    >:: calculate_next_test
          (to_block_matrix identity_matrix_d, 0)
          (to_block_matrix identity_matrix)
          move_down;
    "Testing calculate next on\n   alternate valued matrix with right"
    >:: calculate_next_test
          (to_block_matrix alternate_values_matrix_r, 24)
          (to_block_matrix alternate_values_matrix)
          move_right;
    "Testing\n   calculate next on alternate valued matrix with left"
    >:: calculate_next_test
          (to_block_matrix alternate_values_matrix_l, 24)
          (to_block_matrix alternate_values_matrix)
          move_left;
    "Testing\n   calculate next on alternate valued matrix with up"
    >:: calculate_next_test
          (to_block_matrix alternate_values_matrix_u, 24)
          (to_block_matrix alternate_values_matrix)
          move_up;
    "Testing\n   calculate next on alternate valued matrix with down"
    >:: calculate_next_test
          (to_block_matrix alternate_values_matrix_d, 24)
          (to_block_matrix alternate_values_matrix)
          move_down;
    "Testing\n   calculate next on matrix that doesnt change with right"
    >:: calculate_next_test
          (to_block_matrix no_movement_matrix, 0)
          (to_block_matrix no_movement_matrix)
          move_right;
    "Testing calculate next on matrix that doesnt change with left"
    >:: calculate_next_test
          (to_block_matrix no_movement_matrix, 0)
          (to_block_matrix no_movement_matrix)
          move_left;
  ]

let tests =
  "test suite"
  >::: compress_tests @ merge_tests @ move_tests @ transpose_tests
       @ calculate_next_tests

(* let tests = compress_tests *)
let _ = run_test_tt_main tests
