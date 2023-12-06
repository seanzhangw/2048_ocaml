open OUnit2
open Block
open Block_logic
open Constants

let pp_pair pp_l pp_r (l, r) = Printf.sprintf "(%s, %s)" (pp_l l) (pp_r r)

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  Printf.sprintf "[%s]" (pp_elts lst)

let pp_int_list = pp_list string_of_int
let pp_int_list_int_pair = pp_pair pp_int_list string_of_int

let pp_int_matrix matrix =
  let pp_row row =
    "[" ^ String.concat "; " (List.map string_of_int row) ^ "]"
  in
  "[" ^ String.concat "; " (List.map pp_row matrix) ^ "]"

let pp_matrix_int_pair (matrix, score) =
  Printf.sprintf "(%s, %d)" (pp_int_matrix matrix) score

let compress_test out in1 _ =
  assert_equal ~printer:pp_int_list out (compress in1)

let l_merge_test out in1 _ =
  assert_equal ~printer:(pp_pair pp_int_list string_of_int) out (l_merge in1)

let r_merge_test out in1 _ =
  assert_equal ~printer:(pp_pair pp_int_list string_of_int) out (r_merge in1)

let l_move_test expected input _ =
  assert_equal ~printer:pp_int_list_int_pair
    ~msg:("function: l_move\ninput: " ^ pp_int_list input)
    expected (l_move input)

let r_move_test expected input _ =
  assert_equal ~printer:pp_int_list_int_pair
    ~msg:("function: r_move\ninput: " ^ pp_int_list input)
    expected (r_move input)

let transpose_test out in1 _ =
  assert_equal ~printer:pp_int_matrix out (transpose in1)

let u_move_test out in1 _ =
  assert_equal ~printer:pp_matrix_int_pair out (u_move in1)

let d_move_test out in1 _ =
  assert_equal ~printer:pp_matrix_int_pair out (d_move in1)

let calculate_next_test out in1 in2 _ =
  assert_equal ~printer:pp_matrix_int_pair out (calculate_next in1 in2)

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

let compress_tests =
  [
    "Testing all 0" >:: compress_test [] [];
    "Testing all 0" >:: compress_test [] all_zeros;
    "Testing single zero" >:: compress_test [] single_zero;
    "Testing single non-zero" >:: compress_test single_one_list single_one_list;
    "Testing all non-empty" >:: compress_test ones_list ones_list;
    "Testing non-zero elements only"
    >:: compress_test one_to_four_list one_to_four_list;
    "Testing 0 spreaded out"
    >:: compress_test (List.rev one_two_list) spread_zeros;
    "Testing alternating zeros and non-zeros"
    >:: compress_test one_to_three_list alternating_zeros;
    "Testing all zeros except one"
    >:: compress_test single_one_list all_zeros_except_one;
    "Testing zeros at start" >:: compress_test one_two_list zeros_at_start;
    "Testing zeros at end" >:: compress_test one_two_list zeros_at_end;
    "Testing 0 only on the left"
    >:: compress_test one_to_three_list zero_on_left;
    "Testing 0 only on the right"
    >:: compress_test one_to_three_list zero_on_right;
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

let merge_tests =
  [
    "Testing list of 0s lists with left merge"
    >:: l_merge_test (list_of_two_zeros, 0) list_of_zeros;
    "Basic merge with left merge"
    >:: l_merge_test (basic_merge_left_output, 4) basic_merge_left_input;
    "No merge with left merge"
    >:: l_merge_test (no_merge_left_output, 0) no_merge_left_input;
    "Multiple merges with left merge"
    >:: l_merge_test (multiple_merge_left_output, 12) multiple_merge_left_input;
    "Merge with leading zero in left merge"
    >:: l_merge_test
          (merge_leading_zero_left_output, 4)
          merge_leading_zero_left_input;
    "Testing list of 0s with right merge"
    >:: r_merge_test (list_of_two_zeros, 0) list_of_zeros;
    "Basic merge with right merge"
    >:: r_merge_test (basic_merge_right_output, 4) basic_merge_right_input;
    "No merge with right merge"
    >:: r_merge_test (no_merge_right_output, 0) no_merge_right_input;
    "Multiple merges with right merge"
    >:: r_merge_test
          (multiple_merge_right_output, 12)
          multiple_merge_right_input;
    "Merge with trailing zero in right merge"
    >:: r_merge_test
          (merge_trailing_zero_right_output, 4)
          merge_trailing_zero_right_input;
  ]

let left_move_input = [ 2; 2; 0; 2 ]
let left_move_output = [ 4; 2; 0; 0 ]
let left_move_with_merge_input = [ 2; 2; 0; 0 ]
let left_move_with_merge_output = [ 4; 0; 0; 0 ]
let left_move_no_merge_input = [ 2; 0; 4; 2 ]
let left_move_no_merge_output = [ 2; 4; 2; 0 ]
let left_move_complex_merge_input = [ 2; 2; 2; 2 ]
let left_move_complex_merge_output = [ 4; 4; 0; 0 ]
let right_move_input = [ 2; 2; 0; 2 ]
let right_move_output = [ 0; 0; 2; 4 ]
let right_move_with_merge_input = [ 2; 2; 0; 0 ]
let right_move_with_merge_output = [ 0; 0; 0; 4 ]
let right_move_no_merge_input = [ 2; 0; 4; 2 ]
let right_move_no_merge_output = [ 0; 2; 4; 2 ]
let right_move_complex_merge_input = [ 2; 2; 2; 2 ]
let right_move_complex_merge_output = [ 0; 0; 4; 4 ]

let move_tests =
  [
    "Testing left move" >:: l_move_test (left_move_output, 4) left_move_input;
    "Left move with merge and trailing zeros"
    >:: l_move_test (left_move_with_merge_output, 4) left_move_with_merge_input;
    "Left move with no merge"
    >:: l_move_test (left_move_no_merge_output, 0) left_move_no_merge_input;
    "Left move with complex merge"
    >:: l_move_test
          (left_move_complex_merge_output, 8)
          left_move_complex_merge_input;
    "Testing right move" >:: r_move_test (right_move_output, 4) right_move_input;
    "Right move with merge and leading zeros"
    >:: r_move_test
          (right_move_with_merge_output, 4)
          right_move_with_merge_input;
    "Right move with no merge"
    >:: r_move_test (right_move_no_merge_output, 0) right_move_no_merge_input;
    "Right move with complex merge"
    >:: r_move_test
          (right_move_complex_merge_output, 8)
          right_move_complex_merge_input;
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

let transpose_tests =
  [
    "Testing transpose on identity matrix"
    >:: transpose_test identity_matrix identity_matrix;
    "Testing transpose on zero matrix"
    >:: transpose_test zero_matrix zero_matrix;
    "Testing transpose on non-empty matrix" >:: transpose_test matrix1 matrix1t;
    "Testing transpose on non-empty matrix with zeros"
    >:: transpose_test matrix2 matrix2t;
    "Testing transpose of a single row matrix"
    >:: transpose_test single_row_matrix single_row_matrix_t;
    "Testing transpose of a single column matrix"
    >:: transpose_test single_column_matrix single_column_matrix_t;
    "Testing transpose of a large matrix"
    >:: transpose_test large_matrix large_matrix_t;
    "Testing transpose of an unequal row and column matrix"
    >:: transpose_test unequal_row_col_matrix unequal_row_col_matrix_t;
    "Testing transpose of a matrix with negative values"
    >:: transpose_test negative_values_matrix negative_values_matrix_t;
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

let calculate_next_tests =
  [
    "Testing calculate next on empty matrix with right"
    >:: calculate_next_test (zero_matrix, 0) zero_matrix move_right;
    "Testing calculate next on empty matrix with left"
    >:: calculate_next_test (zero_matrix, 0) zero_matrix move_left;
    "Testing calculate next on empty matrix with up"
    >:: calculate_next_test (zero_matrix, 0) zero_matrix move_up;
    "Testing calculate next on empty matrix with down"
    >:: calculate_next_test (zero_matrix, 0) zero_matrix move_down;
    "Testing calculate next on empty matrix with right"
    >:: calculate_next_test (identity_matrix_r, 0) identity_matrix move_right;
    "Testing calculate next on empty matrix with left"
    >:: calculate_next_test (identity_matrix_l, 0) identity_matrix move_left;
    "Testing calculate next on empty matrix with up"
    >:: calculate_next_test (identity_matrix_u, 0) identity_matrix move_up;
    "Testing calculate next on empty matrix with down"
    >:: calculate_next_test (identity_matrix_d, 0) identity_matrix move_down;
    "Testing calculate next on alternate valued matrix with right"
    >:: calculate_next_test
          (alternate_values_matrix_r, 24)
          alternate_values_matrix move_right;
    "Testing calculate next on alternate valued matrix with left"
    >:: calculate_next_test
          (alternate_values_matrix_l, 24)
          alternate_values_matrix move_left;
    "Testing calculate next on alternate valued matrix with up"
    >:: calculate_next_test
          (alternate_values_matrix_u, 24)
          alternate_values_matrix move_up;
    "Testing calculate next on alternate valued matrix with down"
    >:: calculate_next_test
          (alternate_values_matrix_d, 24)
          alternate_values_matrix move_down;
    "Testing calculate next on matrix that doesnt change with right"
    >:: calculate_next_test (no_movement_matrix, 0) no_movement_matrix
          move_right;
    "Testing calculate next on matrix that doesnt change with left"
    >:: calculate_next_test (no_movement_matrix, 0) no_movement_matrix move_left;
  ]

let tests =
  "test suite"
  >::: compress_tests @ merge_tests @ move_tests @ transpose_tests
       @ calculate_next_tests

let _ = run_test_tt_main tests
