open Raylib
open Color
open Board
open Start
open Constants
open Instructions
open Block_logic
open L_state
open Win_state

type game_state =
  | StartingPage
  | Game
  | InstructionsPage
  | Lost
  | Won
  | ContinuePlaying

(* Initiates the RayLib window with window size and frame rate *)
let setup () =
  init_window 800 600 "raylib [core] example - basic window";
  set_target_fps 60

(* Draws and implements the logic for the start page. Continuously checks for
   key input to progress to instructions or game state *)
let starting_page_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  starting_page ();
  let next_state =
    if is_key_pressed Key.Space then Game
    else if is_key_pressed Key.I then InstructionsPage
    else StartingPage
  in
  end_drawing ();
  next_state

(** Stores the data we are displaying for the board *)
let board =
  ref [ [ 0; 0; 2; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 512; 512; 512; 512 ] ]

(* * Stores the current score let score = "0" *)

(** Logic behind handling the button click for the new game button *)
let check_new_game_button_click () =
  (* If the mouse is over the button and the left mouse button is pressed *)
  if Raylib.is_mouse_button_pressed MouseButton.Left then
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= 600
      && mouse_x <= 600 + 150
      && mouse_y >= 100
      && mouse_y <= 100 + 50
    then
      (* Reset the board to all zeroes *)
      board :=
        [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]

let score = ref 0

let rec check_foldable_row t = (* Given a 
   list of integers, checks to see if there
    are any consecutive equal pairs. Returns
   false if there are none, and true if
    some exist. *)
   print_endline "I am now checking this row:";
   Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int t));
  match t with
  | [] -> false
  | h :: [] -> false
  | a :: (b :: _ as t) -> a = b || check_foldable_row t

let rec check_row_foldable s = (* Given a board, 
checks to see if it can be moved left or right. Returns
  true if it is able to; returns false if otherwise. *)
    print_endline "I am now checking this board:";
  List.iter (fun l -> Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int l))) s;
  match s with
  | [] -> false
  | h :: [] -> check_foldable_row h
  | h :: (t :: _ as x) -> if check_foldable_row h then true else (check_row_foldable x)

let check_col_foldable (matrix : int list list) = (* Given a board, 
checks to see if it can be moved up or down. Returns
  true if it is able to; returns false if otherwise. *)
    let transpose matrix =
      let rec transposer x y =
        match y with
        | [] | [] :: _ -> List.rev x
        | _ ->
          let new_row = List.map List.hd y in
          let y = List.map List.tl y in
          transposer (new_row :: x) y
      in
      transposer [] matrix
    in 
    print_endline "Columns board:";
    List.iter (fun l -> Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int l))) (transpose matrix);
    check_row_foldable (transpose matrix)

let check_foldable (matrix : int list list) =
  (* Given a board, 
checks to see if any further moves on it can be made. Returns true if so and returns false otherwise. *)
  check_row_foldable matrix && check_col_foldable matrix

let find_2048 (board : int list list) : bool = (* Given a board, sees if any 
   of the elements of the board are 2048. Returns true or false*)
  List.exists (fun row -> List.exists ((=) 2048) row) board 

(* Draws and implements the logic for the game page. Continuously checks for key
   input to reset the game *)
let game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  check_new_game_button_click ();

  let handle_move dir =
    let new_board, score_delta = calculate_next !board dir in 

   if (* checks if game board is invalid *)
      count_empty (find_zeros new_board) = 0 &&
      check_foldable new_board = false then
        Lost
    else if  (* checks if the board has 2048 *)
      find_2048 new_board then (*  && won_alr = false then *)
        (let final_board = generate_block new_board in
        score := !score + score_delta;
        board := final_board;
        Won)
    else (
      let final_board = generate_block new_board in
    board := final_board;
    Game)
    (* else
      print_endline "LOSING STATE!!!!!" *)

  in

  let next_state =
  if is_key_pressed Key.Left then handle_move move_left
  else if is_key_pressed Key.Right then handle_move move_right
  else if is_key_pressed Key.Up then handle_move move_up
  else if is_key_pressed Key.Down then handle_move move_down
  else Game;
  in

  display_tiles_input !board;
  draw_text "Score: " 550 30 30 Color.brown;
  draw_text (string_of_int !score) 670 30 30 Color.beige;

  end_drawing ();
  next_state
  
let lost_state () = 
  begin_drawing ();
  clear_background Color.raywhite;
  lose_state ();
  let next_state =
    if is_key_pressed Key.Escape then StartingPage
    else if is_key_pressed Key.S then Game
    else Lost
  in
  end_drawing ();
  next_state

let won_state () = 
  begin_drawing ();
  clear_background Color.raywhite;
  win_state ();
  let next_state =
    if is_key_pressed Key.Escape then StartingPage
    else if is_key_pressed Key.S then Game
    else if is_key_pressed Key.D then ContinuePlaying
    else Won
  in
  end_drawing ();
  next_state


let continue_playing_state () =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  check_new_game_button_click ();

  let handle_move dir =
    let new_board, score_delta = calculate_next !board dir in 

   if (* checks if game board is invalid *)
      count_empty (find_zeros new_board) = 0 &&
      check_foldable new_board = false then
        Lost
    else (
      let final_board = generate_block new_board in
    board := final_board;
    ContinuePlaying)

  in

  let next_state =
  if is_key_pressed Key.Left then handle_move move_left
  else if is_key_pressed Key.Right then handle_move move_right
  else if is_key_pressed Key.Up then handle_move move_up
  else if is_key_pressed Key.Down then handle_move move_down
  else ContinuePlaying;
  in

  display_tiles_input !board;
  draw_text "Score: " 550 30 30 Color.brown;
  draw_text (string_of_int !score) 670 30 30 Color.beige;

  end_drawing ();
  next_state

(* Draws and implements the logic for the instruction page. Continuously checks
   for key input to return to start page or begin the game *)
let instructions_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  Instructions.instructions ();
  let next_state =
    if is_key_pressed Key.Escape then StartingPage
    else if is_key_pressed Key.S then Game
    else InstructionsPage
  in
  end_drawing ();
  next_state

(* Main control loop of the game. Depending on the state of the game, a
   different logic block is executed *)
let rec main_loop state =
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let next_state =
      match state with
      | StartingPage -> starting_page_logic ()
      | Game -> game_logic ()
      | InstructionsPage -> instructions_logic ()
      | Lost -> lost_state ()
      | Won -> won_state ()
      | ContinuePlaying -> continue_playing_state ()
    in
    main_loop next_state

let () = setup ()
(* start the main loop with the StartingPage state *)
