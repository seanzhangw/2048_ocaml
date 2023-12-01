open Raylib
open Color
open Board
open Start
open Constants
open Instructions
open Block_logic

type game_state =
  | StartingPage
  | Game
  | InstructionsPage

let score = ref 0
let high_score = ref 0

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

(** Stores the data we are displaying for the board. Generates an initial block
    in a random position *)
let board =
  ref
    (generate_initial
       [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ])

(* * Stores the current score let score = "0" *)

(** Logic behind handling the button click for the new game button *)
let check_new_game_button_click () =
  (* If the mouse is over the button and the left mouse button is pressed *)
  if Raylib.is_mouse_button_pressed MouseButton.Left then (
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
        generate_initial
          [ [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ]; [ 0; 0; 0; 0 ] ];
    score := 0)

(* Draws and implements the logic for the game page. Continuously checks for key
   input to reset the game *)
let game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
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

  display_tiles_input !board;
  draw_text "Score: " 530 30 30 Color.brown;
  draw_text (string_of_int !score) 730 30 30 Color.beige;
  draw_text "High Score: " 530 70 30 Color.brown;
  draw_text (string_of_int !score) 730 70 30 Color.beige;

  end_drawing ();
  Game (* You can transition to another state here if needed *)

(* Draws and implements the logic for the instruction page. Continuously checks
   for key input to return to start page or begin the game *)
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
    in
    main_loop next_state

let () = setup ()
(* start the main loop with the StartingPage state *)
