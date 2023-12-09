open Raylib
open Color
open Board
open Block
open Constants
open Instructions
open Start
open Block_logic

type game_state =
  | StartingPage
  | Game
  | InstructionsPage

let score = ref 0
let high_score = ref 0
let last_move_time = ref 0.
let board = ref (generate_initial ())
let current_message = ref "" (* Added global ref for the current message *)
let reset_current_message () = current_message := ""

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
      reset_current_message ();
      (* Reset the message *)
      Utils.write_to_file Constants.file_path (string_of_int !high_score);
      StartingPage)
    else state
  else state

(** Handles the button click logic for the new game button. *)
let check_new_game_button_click () =
  if Raylib.is_mouse_button_pressed MouseButton.Left then
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= new_pos_x
      && mouse_x <= new_pos_x + new_width
      && mouse_y >= new_pos_y
      && mouse_y <= new_pos_y + new_height
    then (
      board := generate_initial ();
      score := 0;
      reset_current_message ();
      (* Reset the message *)
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
    "You're doing well!";
    "Awesome!";
    "Fantastic!";
    "Superb effort!";
    "You're on fire!";
    "Incredible skill!";
    "Way to go!";
    "You're a natural!";
    "Brilliant move!";
    "You're smashing it!";
    "Exceptional strategy!";
    "You're a genius!";
    "Unstoppable!";
    "Amazing progress!";
    "Keep the streak going!";
    "Masterful play!";
    "You're a superstar!";
    "Phenomenal!";
    "Setting records!";
    "You make it look easy!";
    "You're a champion!";
    "You're leading the way!";
    "Spectacular!";
  ]

let encouragement_text () =
  Raylib.draw_text !current_message encouragement_text_pos_x
    encouragement_text_pos_y encouragement_text_size Color.brown

(** Handles the game logic, checking for key input, button clicks, and updating
    the board. *)
let rec game_logic current_time delta_time =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  let next_state = check_home_page_button_click Game in
  check_new_game_button_click ();
  encouragement_text ();
  if !score > !high_score then high_score := !score
  else high_score := !high_score;
  if is_key_pressed Key.Left then handle_move current_time move_left
  else if is_key_pressed Key.Right then handle_move current_time move_right
  else if is_key_pressed Key.Up then handle_move current_time move_up
  else if is_key_pressed Key.Down then handle_move current_time move_down;
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

and handle_move current_time dir =
  if current_time -. !last_move_time > Constants.move_cooldown then (
    last_move_time := current_time;
    let new_board, score_delta = calculate_next !board dir in
    if new_board = !board then (
      score := !score + score_delta;
      board := new_board)
    else
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      (* Update the current message only when a move is made *)
      current_message :=
        List.nth encouraging_messages
          (Random.int (List.length encouraging_messages)))
  else ()

(* Function to reset the current message *)
let reset_current_message () = current_message := ""

(** Handles the logic for the instruction page, checking for key input to return
    to the start page or begin the game. *)
let instructions_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  instructions ();
  let next_state =
    if is_key_pressed Key.Escape then StartingPage
    else if is_key_pressed Key.S then Game
    else InstructionsPage
  in
  end_drawing ();
  next_state

(** Main control loop of the game, executing different logic blocks based on the
    current game state. *)
let rec main_loop last_time state =
  let current_time = Unix.gettimeofday () in
  let delta_time = current_time -. last_time in
  if Raylib.window_should_close () then Raylib.close_window ()
  else
    let next_state =
      match state with
      | StartingPage -> starting_page_logic ()
      | Game -> game_logic current_time delta_time
      | InstructionsPage -> instructions_logic ()
    in
    let frame_end_time = Unix.gettimeofday () in
    let frame_duration = frame_end_time -. current_time in
    let frame_target = 1.0 /. float_of_int fps in
    let sleep_duration = max 0.0 (frame_target -. frame_duration) in
    ignore (Unix.select [] [] [] sleep_duration);

    main_loop current_time next_state

(* Start the main loop with the StartingPage state *)
let () = setup ()
