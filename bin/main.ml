(* main.ml *)

(* Import the Game module. This assumes game.ml is in the same directory. *)
open G2048
open Game

(* Now, you can use functions from the Game module *)
let () =
  setup ();
  (* Call the setup function from Game module *)
  main_loop StartingPage (* Call the loop function from Game module *)
