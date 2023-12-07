open Raylib
open Color
open Board
open Block
open Constants
open Instructions
open Start
open Block_logic
open Tetris_logic

type game_state =
  | StartingPage
  | Game
  | InstructionsPage
  | Tetris

let score = ref 0
let high_score = ref 0

(*Stores the data we are displaying for the board.*)
let board = ref (generate_initial ())

(* Initiates the RayLib window with window size and frame rate *)
let setup () =
  init_window 800 600 "raylib [core] example - basic window";
  set_target_fps 60

(** Stores the data we are displaying for the board. Generates an initial block
    in a random position *)
(* let board =
  ref
    (generate_initial
       [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ]) *)

let tetris_board = ref (Array.init rows (fun _ -> Array.make columns 0))
let is_tetris_initialized = ref false

(* Placeholder initialization *)
let init_board () = Array.make_matrix 5 4 0

(* Function to initialize the Tetris board *)
let init_tetris_board () =
  if not !is_tetris_initialized then begin
    tetris_board := init_board ();
    (* add_new_block_in_top_row !tetris_board; *)
    is_tetris_initialized := true
  end

(* Draws and implements the logic for the start page. Continuously checks for
   key input to progress to instructions or game state *)
   let starting_page_logic () =
    begin_drawing ();
    clear_background Color.raywhite;
    starting_page ();
    let next_state =
      if is_key_pressed Key.I then InstructionsPage else StartingPage
    in
    end_drawing ();
    next_state
  
    let check_home_page_button_click state =
      if Raylib.is_mouse_button_pressed MouseButton.Left then
        let mouse_x = Raylib.get_mouse_x () in
        let mouse_y = Raylib.get_mouse_y () in
        if
          mouse_x >= 37
          && mouse_x <= 37 + 184
          && mouse_y >= 30
          && mouse_y <= 30 + 56
        then (
          (* Reset the board *)
          board := generate_initial ();
          score := 0;
          Utils.write_to_file Constants.file_path (string_of_int !high_score);
          StartingPage
        )
        else state
      else state
(** Logic behind handling the button click for the new game button *)
let check_new_game_button_click () =
  (* If the mouse is over the button and the left mouse button is pressed *)
  if Raylib.is_mouse_button_pressed MouseButton.Left then (
    let mouse_x = Raylib.get_mouse_x () in
    let mouse_y = Raylib.get_mouse_y () in
    if
      mouse_x >= 615
      && mouse_x <= 615 + 150
      && mouse_y >= 37
      && mouse_y <= 37 + 58
    then (* Reset the board *)
      board := generate_initial ();
    score := 0;
    Utils.write_to_file Constants.file_path (string_of_int !high_score))

let animate delta_time =
  List.iter
    (fun row ->
      List.iter
        (fun block ->
          match block.state with
          | Stationary -> ()
          | _ -> update_block block delta_time)
        row)
    !board;

  display_tiles_input !board

let new_board tetris_board block =
  if Array.length tetris_board > 0 then tetris_board.(0) <- Array.of_list block

let tetris_game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  tetris_page ();
  let next_state = check_home_page_button_click Tetris in
  (* Update the state based on button click *)
  check_new_game_button_click ();
  new_board !tetris_board (generate_random_block ());
  update_board !tetris_board;

  end_drawing ();
  next_state

(* Return the updated state *)
(* Maintain TetrisGame state or transition to others if needed *)

(*Check is button clicked to return to the homepage*)
(* let check_home_page_button_click () = if Raylib.is_mouse_button_pressed
   MouseButton.Left then let mouse_x = Raylib.get_mouse_x () in let mouse_y =
   Raylib.get_mouse_y () in if mouse_x >= 37 && mouse_x <= 37 + 184 && mouse_y
   >= 30 && mouse_y <= 30 + 56 then StartingPage else Game else Game *)

let new_board tetris_board block =
  if Array.length tetris_board > 0 then tetris_board.(0) <- Array.of_list block

let tetris_game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  tetris_page ();
  let next_state = check_home_page_button_click Tetris in
  (* Update the state based on button click *)
  check_new_game_button_click ();
  new_board !tetris_board (generate_random_block ());
  update_board !tetris_board;

  end_drawing ();
  next_state

(* Return the updated state *)
(* Maintain TetrisGame state or transition to others if needed *)

(*Check is button clicked to return to the homepage*)
(* let check_home_page_button_click () = if Raylib.is_mouse_button_pressed
   MouseButton.Left then let mouse_x = Raylib.get_mouse_x () in let mouse_y =
   Raylib.get_mouse_y () in if mouse_x >= 37 && mouse_x <= 37 + 184 && mouse_y
   >= 30 && mouse_y <= 30 + 56 then StartingPage else Game else Game *)

(* Draws and implements the logic for the game page. Continuously checks for key
   input to reset the game *)
let game_logic delta_time =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  let next_state = check_home_page_button_click Game in
  (* Update the state based on button click *)
  check_new_game_button_click ();

  let handle_move dir =
    let new_board, score_delta = calculate_next !board dir in
    if new_board = !board then (
      score := !score + score_delta;
      board := new_board)
    else
      let final_board = generate_block new_board in
      score := !score + score_delta;
      board := final_board
  in
  if !score > !high_score then high_score := !score
  else high_score := !high_score;

  if is_key_pressed Key.Left then handle_move move_left
  else if is_key_pressed Key.Right then handle_move move_right
  else if is_key_pressed Key.Up then handle_move move_up
  else if is_key_pressed Key.Down then handle_move move_down;
  animate delta_time;
  display_tiles_input !board;
  draw_text "Score " 300 37 15 Color.brown;
  draw_text (string_of_int !score) 300 57 47 Color.beige;
  draw_text "High Score " 450 37 15 Color.brown;
  draw_text (string_of_int !high_score) 450 57 47 Color.beige;

  end_drawing ();
  next_state (* Return the updated state *)

(* Draws and implements the logic for the instruction page. Continuously checks
   for key input to return to start page or begin the game *)
let instructions_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  instructions ();
  let next_state =
    if is_key_pressed Key.Escape then StartingPage
    else if is_key_pressed Key.O then Game
    else if is_key_pressed Key.T then Tetris
    else InstructionsPage
  in
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
      | Game -> game_logic delta_time
      | InstructionsPage -> instructions_logic ()
      | Tetris -> tetris_game_logic ()
    in
    let frame_end_time = gettimeofday () in
    let frame_duration = frame_end_time -. current_time in
    let frame_target = 1.0 /. 60.0 in
    let sleep_duration = max 0.0 (frame_target -. frame_duration) in
    ignore (select [] [] [] sleep_duration);

    main_loop current_time next_state

let () = setup ()
(* start the main loop with the StartingPage state *)
