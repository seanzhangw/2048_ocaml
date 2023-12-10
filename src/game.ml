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

let board =
  ref
    [
      [
        place_block 1024 (0, 0);
        place_block 1024 (1, 0);
        place_block 0 (2, 0);
        place_block 0 (3, 0);
      ];
      [
        place_block 0 (0, 1);
        place_block 0 (1, 1);
        place_block 0 (2, 1);
        place_block 0 (3, 1);
      ];
      [
        place_block 0 (0, 2);
        place_block 0 (1, 2);
        place_block 0 (2, 2);
        place_block 0 (3, 2);
      ];
      [
        place_block 0 (0, 3);
        place_block 0 (1, 3);
        place_block 0 (2, 3);
        place_block 0 (3, 3);
      ];
    ]

let current_message = ref ""
let current_message_pos = ref (60, 100)

let current_message_size =
  ref
    (List.nth encouragement_text_size
       (Random.int (List.length encouragement_text_size)))

let reset () =
  score := 0;
  board := generate_initial ();
  Utils.write_to_file Constants.file_path (string_of_int !high_score)

let setup () =
  init_window screen_width screen_height "raylib [core] example - basic window";
  set_target_fps fps

let init_board () = Array.make_matrix 5 4 0

let starting_page_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  starting_page ();
  let next_state =
    if is_key_pressed Key.I then InstructionsPage else StartingPage
  in
  end_drawing ();
  next_state

let check_home_page_button_click () =
  if Raylib.is_mouse_button_pressed MouseButton.Left then
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= home_pos_x
      && mouse_x <= home_pos_x + home_width
      && mouse_y >= home_pos_y
      && mouse_y <= home_pos_y + home_height
    then (
      reset ();
      true)
    else false
  else false

let check_new_game_button_click () =
  if Raylib.is_mouse_button_pressed MouseButton.Left then (
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= new_pos_x
      && mouse_x <= new_pos_x + new_width
      && mouse_y >= new_pos_y
      && mouse_y <= new_pos_y + new_height
    then (
      board := generate_initial ();
      score := 0);
    Utils.write_to_file Constants.file_path (string_of_int !high_score))

let animate delta_time =
  List.iter
    (fun row ->
      List.iter (fun block -> Block.update_block block delta_time) row)
    !board;

  display_tiles_input !board

let encouragement_text () =
  Raylib.draw_text !current_message (fst !current_message_pos)
    (snd !current_message_pos) !current_message_size Color.brown

let find_2048 (board : block list list) : bool =
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
  print_endline "GAME STATE";
  if current_time -. !last_move_time > Constants.move_cooldown then (
    last_move_time := current_time;
    let new_board, score_delta = calculate_next !board dir in

    if not (Block.equal new_board !board) then (
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      if find_2048 final_board then Won
      else if check_end final_board then Lost
      else (
        current_message :=
          List.nth encouraging_messages
            (Random.int (List.length encouraging_messages));
        current_message_pos :=
          List.nth encouragement_text_pos
            (Random.int (List.length encouragement_text_pos));
        current_message_size :=
          List.nth encouragement_text_size
            (Random.int (List.length encouragement_text_size));
        Game))
    else if check_end !board then Lost
    else Game)
  else Game

let lost_state () =
  begin_drawing ();
  clear_background Color.raywhite;
  lose_state ();
  let next_state =
    if is_key_pressed Key.S then (
      reset ();
      StartingPage)
    else Lost
  in
  end_drawing ();
  next_state

let won_state () =
  begin_drawing ();
  clear_background Color.raywhite;
  win_state ();
  let next_state =
    if is_key_pressed Key.S then (
      reset ();
      StartingPage)
    else if is_key_pressed Key.D then ContinuePlaying
    else Won
  in
  end_drawing ();
  next_state

let rec continue_playing_state current_time delta_time =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  check_new_game_button_click ();

  if !score > !high_score then high_score := !score
  else high_score := !high_score;
  encouragement_text ();
  let next_state =
    if is_key_pressed Key.Left then handle_move' current_time move_left
    else if is_key_pressed Key.Right then handle_move' current_time move_right
    else if is_key_pressed Key.Up then handle_move' current_time move_up
    else if is_key_pressed Key.Down then handle_move' current_time move_down
    else if check_home_page_button_click () then StartingPage
    else ContinuePlaying
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

and handle_move' current_time dir : game_state =
  if current_time -. !last_move_time > Constants.move_cooldown then (
    last_move_time := current_time;
    let new_board, score_delta = calculate_next !board dir in

    if not (Block.equal new_board !board) then (
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board;
      if check_end final_board then Lost
      else (
        current_message :=
          List.nth encouraging_messages
            (Random.int (List.length encouraging_messages));
        current_message_pos :=
          List.nth encouragement_text_pos
            (Random.int (List.length encouragement_text_pos));
        current_message_size :=
          List.nth encouragement_text_size
            (Random.int (List.length encouragement_text_size));
        ContinuePlaying))
    else if check_end !board then Lost
    else ContinuePlaying)
  else ContinuePlaying

let instructions_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  Instructions.instructions ();
  let next_state = if is_key_pressed Key.S then Game else InstructionsPage in
  end_drawing ();
  next_state

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
      | ContinuePlaying -> continue_playing_state current_time delta_time
    in
    let frame_end_time = gettimeofday () in
    let frame_duration = frame_end_time -. current_time in
    let frame_target = 1.0 /. float_of_int fps in
    let sleep_duration = max 0.0 (frame_target -. frame_duration) in
    ignore (select [] [] [] sleep_duration);

    main_loop current_time next_state

let () = setup ()
