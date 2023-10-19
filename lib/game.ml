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

let setup () =
  init_window 800 600 "raylib [core] example - basic window"; 
  set_target_fps 60

let starting_page_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  starting_page ();
  let next_state = 
    if is_key_pressed Key.Space then
      Game
    else if is_key_pressed Key.I then
      InstructionsPage
    else
      StartingPage
  in
  end_drawing ();
  next_state

let game_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  game_page ();
  end_drawing ();
  Game (* You can transition to another state here if needed *)

let instructions_logic () =
  begin_drawing ();
  clear_background Color.raywhite;
  instructions ();
  let next_state = 
    if is_key_pressed Key.Escape then
      StartingPage
    else
      InstructionsPage
  in
  end_drawing ();
  next_state

let rec main_loop state =
  if Raylib.window_should_close () then 
    Raylib.close_window ()
  else
    let next_state = match state with
      | StartingPage -> starting_page_logic ()
      | Game -> game_logic ()
    
      | InstructionsPage -> instructions_logic ()
    in
    main_loop next_state

let () = 
    setup ();
      (* start the main loop with the StartingPage state *)
