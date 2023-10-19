open Raylib
open Color

let setup () =
    Raylib.init_window 800 400 "raylib [core] example - basic window";
    Raylib.set_target_fps 60

let starting_page () =
    Raylib.draw_text "Welcome to My Game!" 190 100 20 Color.red;
    Raylib.draw_text "Press Space to Start!" 190 130 20 Color.blue

    let rec loop game_started =
      if Raylib.window_should_close () then 
          Raylib.close_window ()
      else
          let open Raylib in
          begin_drawing ();
          clear_background Color.raywhite;
          let new_game_state = 
              if game_started then (
                  draw_text "Game Started! Do game logic here." 190 200 20 Color.green;
                  game_started
              ) else if is_key_pressed Key.Space then
                  true  (* update game_started to true *)
              else (
                  starting_page ();
                  game_started
              )
          in
          end_drawing ();
          loop new_game_state  (* continue looping with updated game_started state *)(* continue looping with current game_started state *)

let () = setup ()  (* start the game loop with game_started set to false *)
