open OUnit2
open Block
open Block_logic

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

let compress_tests =
  [
    "Testing all 0" >:: compress_test [] [];
    "Testing all 0" >:: compress_test [] [ 0; 0; 0; 0 ];
    "Testing all non-empty" >:: compress_test [ 1; 1; 1; 1 ] [ 1; 1; 1; 1 ];
    "Testing 0 spreaded out" >:: compress_test [ 2; 1 ] [ 2; 0; 1; 0 ];
    "Testing 0 only on the left" >:: compress_test [ 1; 2; 3 ] [ 0; 1; 2; 3 ];
    "Testing 0 only on the right" >:: compress_test [ 1; 2; 3 ] [ 1; 2; 3; 0 ];
  ]

let merge_tests =
  [
    "Testing list of 0s lists with left merge"
    >:: l_merge_test ([ 0; 0 ], 0) [ 0; 0; 0; 0 ];
    "Testing list of 0s with right merge"
    >:: r_merge_test ([ 0; 0 ], 0) [ 0; 0; 0; 0 ];
  ]

let move_tests =
  [
    "Testing left move" >:: l_move_test ([ 4; 2; 0; 0 ], 4) [ 2; 2; 0; 2 ];
    "Testing right move" >:: r_move_test ([ 0; 0; 2; 4 ], 4) [ 2; 2; 0; 2 ];
  ]

let transpose_tests = []
let calculate_next_tests = []

let tests =
  "test suite"
  >::: compress_tests @ merge_tests @ move_tests @ transpose_tests
       @ calculate_next_tests

let _ = run_test_tt_main tests
