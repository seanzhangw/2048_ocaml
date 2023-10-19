open Raylib
open Color
open Board
open Start
open Constants

let setup () =
  init_window 800 600 "raylib [core] example - basic window";
  set_target_fps 60

let rec starting_page_loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.raywhite;
  starting_page ();
  if is_key_pressed Key.Space then game_loop () else end_drawing ();
  starting_page_loop ()

and game_loop () =
  if Raylib.window_should_close () then Raylib.close_window ()
  else begin_drawing ();
  clear_background Color.raywhite;
  draw_grid ();
  end_drawing ();
  game_loop ()

let () = setup ()
(* set up the game *)
(* start the game loop with game_started set to false *)
