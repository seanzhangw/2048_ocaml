open Raylib
open Color
open Board
open Start
open Constants
open Instructions

type game_state =
  | StartingPage
  | Game
  | InstructionsPage

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

(* Draws and implements the logic for the game page. Continuously checks for key
   input to reset the game *)
let game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  check_new_game_button_click ();
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
