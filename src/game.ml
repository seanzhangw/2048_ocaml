open Raylib
open Color
open Board
open Block
open Constants
open Instructions
open Start
open Block_logic
open L_state
open Win_state
open Block

type game_state =
  | StartingPage
  | Game
  | InstructionsPage
  | Lost
  | Won
  | ContinuePlaying

let score = ref 0
let high_score = ref 0
let last_move_time = ref 0.
let board = ref (generate_initial ())
let current_message = ref ""
let current_message_pos = ref (60, 100)

(** Initializes the Raylib window with the specified size and frame rate. *)
let setup () =
  init_window screen_width screen_height "raylib [core] example - basic window";
  set_target_fps fps

(** Placeholder initialization for the game board. *)
let init_board () = Array.make_matrix 5 4 0

(** Handles the logic for the starting page, checking for key input to progress
    to instructions or the game state. *)
let starting_page_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  starting_page ();
  let next_state =
    if is_key_pressed Key.I then InstructionsPage else StartingPage
  in
  end_drawing ();
  next_state

(** Checks for the home page button click and resets the board if clicked. *)
let check_home_page_button_click state =
  if Raylib.is_mouse_button_pressed MouseButton.Left then
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= home_pos_x
      && mouse_x <= home_pos_x + home_width
      && mouse_y >= home_pos_y
      && mouse_y <= home_pos_y + home_height
    then (
      board := generate_initial ();
      score := 0;
      Utils.write_to_file Constants.file_path (string_of_int !high_score);
      true)
    else false
  else false

(** Handles the button click logic for the new game button. *)
let check_new_game_button_click () =
  if Raylib.is_mouse_button_pressed MouseButton.Left then (
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= new_pos_x
      && mouse_x <= new_pos_x + new_width
      && mouse_y >= new_pos_y
      && mouse_y <= new_pos_y + new_height
    then board := generate_initial ();
    score := 0;
    Utils.write_to_file Constants.file_path (string_of_int !high_score))

(** Animates the game board based on the elapsed time. *)
let animate delta_time =
  List.iter
    (fun row ->
      List.iter (fun block -> Block.update_block block delta_time) row)
    !board;

  display_tiles_input !board

let encouraging_messages =
  [
    "Great move!";
    "Keep it up!";
    "Nice one!";
    "Awesome!";
    "Fantastic!";
    "Superb effort!";
    "Superb!";
    "Great!";
    "Slay!";
    "You're on fire!";
    "Incredible skill!";
    "Way to go!";
    "You're a natural!";
    "Brilliant!";
    "Wonderful!";
    "Exceptional!";
    "You're a genius!";
    "Unstoppable!";
    "Amazing!";
    "Masterful play!";
    "Superstar!";
    "Phenomenal!";
    "Setting records!";
    "You're a champ!";
    "Spectacular!";
  ]

let encouragement_text_pos =
  [ (60, 100); (100, 110); (350, 100); (60, 550); (100, 550); (350, 550) ]

let encouragement_text () =
  Raylib.draw_text !current_message (fst !current_message_pos)
    (snd !current_message_pos) encouragement_text_size Color.brown

let reset_current_message () = current_message := ""

let rec check_foldable_row t =
  (* Given a list of blocks, checks to see if there are any consecutive equal
     pairs of values. Returns false if there are none, and true if some
     exist. *)
  match t with
  | [] -> false
  | h :: [] -> false
  | a :: (b :: _ as t) ->
      Block.get_value a = Block.get_value b || check_foldable_row t

let rec check_row_foldable s =
  (* Given a board, checks to see if it can be moved left or right. Returns true
     if it is able to; returns false if otherwise. *)
  match s with
  | [] -> false
  | h :: [] -> check_foldable_row h
  | h :: (t :: _ as x) ->
      if check_foldable_row h then true else check_row_foldable x

let check_col_foldable matrix =
  (* Given a board, checks to see if it can be moved up or down. Returns true if
     it is able to; returns false if otherwise. *)
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
  check_row_foldable (transpose matrix)

let check_foldable matrix =
  (* Given a board, checks to see if any further moves on it can be made.
     Returns true if so and returns false otherwise. *)
  check_row_foldable matrix && check_col_foldable matrix

let find_2048 (board : block list list) : bool =
  (* Given a board, sees if any of the elements of the board are 2048. Returns
     true or false*)
  List.exists
    (fun row -> List.exists (fun block -> get_value block = 2048) row)
    board

let rec game_logic current_time delta_time =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();

  check_new_game_button_click ();

  if !score > !high_score then high_score := !score
  else high_score := !high_score;
  encouragement_text ();
  let next_state =
    if is_key_pressed Key.Left then handle_move current_time move_left
    else if is_key_pressed Key.Right then handle_move current_time move_right
    else if is_key_pressed Key.Up then handle_move current_time move_up
    else if is_key_pressed Key.Down then handle_move current_time move_down
    else if check_home_page_button_click () then StartingPage
    else Game
  in
  animate delta_time;

  display_tiles_input !board;
  draw_text "Score " score_label_pos_x score_label_pos_y score_label_size
    Color.brown;
  draw_text (string_of_int !score) score_pos_x score_pos_y score_size
    Color.beige;
  draw_text "High Score " hs_label_pos_x hs_label_pos_y hs_label_size
    Color.brown;
  draw_text (string_of_int !high_score) hs_pos_x hs_pos_y hs_size Color.beige;

  end_drawing ();
  next_state

and handle_move current_time dir : game_state =
  if current_time -. !last_move_time > Constants.move_cooldown then (
    last_move_time := current_time;
    let new_board, score_delta = calculate_next !board dir in

    if
      (* checks if game board is invalid *)
      count_empty new_board = 0 && check_foldable new_board = false
    then Lost
    else if (* checks if the board has 2048 *)
            find_2048 new_board then (
      (* && won_alr = false then *)
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      Won)
    else if
      (* checks if game board is invalid *)
      count_empty new_board = 0 && check_foldable new_board = false
    then Lost
    else if (* checks if the board has 2048 *)
            find_2048 new_board then (
      (* && won_alr = false then *)
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      Won)
    else if new_board = !board then (
      score := !score + score_delta;
      board := new_board;
      Game)
    else
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      current_message :=
        List.nth encouraging_messages
          (Random.int (List.length encouraging_messages));
      current_message_pos :=
        List.nth encouragement_text_pos
          (Random.int (List.length encouragement_text_pos));
      Game)
  else Game

let lost_state () =
  begin_drawing ();
  clear_background Color.raywhite;
  lose_state ();
  let next_state = if is_key_pressed Key.S then StartingPage else Lost in
  end_drawing ();
  next_state

let won_state () =
  begin_drawing ();
  clear_background Color.raywhite;
  win_state ();
  let next_state =
    if is_key_pressed Key.S then StartingPage
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

    if
      (* checks if game board is invalid *)
      count_empty new_board = 0 && check_foldable new_board = false
    then Lost
    else
      let final_board = generate_block new_board in
      board := final_board;
      ContinuePlaying
  in

  let next_state =
    if is_key_pressed Key.Left then handle_move move_left
    else if is_key_pressed Key.Right then handle_move move_right
    else if is_key_pressed Key.Up then handle_move move_up
    else if is_key_pressed Key.Down then handle_move move_down
    else ContinuePlaying
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
  let next_state = if is_key_pressed Key.S then Game else InstructionsPage in
  end_drawing ();
  next_state

(* Main control loop of the game. Depending on the state of the game, a
   different logic block is executed *)
let rec main_loop last_time state =
  let open Unix in
  let current_time = gettimeofday () in
  let delta_time = current_time -. last_time in
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let next_state =
      match state with
      | StartingPage -> starting_page_logic ()
      | Game -> game_logic current_time delta_time
      | InstructionsPage -> instructions_logic ()
      | Lost -> lost_state ()
      | Won -> won_state ()
      | ContinuePlaying -> continue_playing_state ()
    in
    let frame_end_time = gettimeofday () in
    let frame_duration = frame_end_time -. current_time in
    let frame_target = 1.0 /. float_of_int fps in
    let sleep_duration = max 0.0 (frame_target -. frame_duration) in
    ignore (select [] [] [] sleep_duration);

    main_loop current_time next_state

let () = setup ()
(* start the main loop with the StartingPage state *)
