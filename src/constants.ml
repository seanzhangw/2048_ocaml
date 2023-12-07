(* Screen dimensions *)
let screen_width = 800
let screen_height = 600
let num_squares = 4
let square_size = 80
let spacing = 7

(* Control constants *)
let move_left = 0
let move_right = 1
let move_up = 2
let move_down = 3

(* High score *)
let file_path = "data/high_score.json"

(* Animation *)
let block_speed = 2.0

let block_position_mapping = function
  | 0, 0 -> (229., 149.)
  | 1, 0 -> (316., 149.)
  | 2, 0 -> (403., 149.)
  | 3, 0 -> (490., 149.)
  | 0, 1 -> (229., 236.)
  | 1, 1 -> (316., 236.)
  | 2, 1 -> (403., 236.)
  | 3, 1 -> (490., 236.)
  | 0, 2 -> (229., 323.)
  | 1, 2 -> (316., 323.)
  | 2, 2 -> (403., 323.)
  | 3, 2 -> (490., 323.)
  | 0, 3 -> (229., 410.)
  | 1, 3 -> (316., 410.)
  | 2, 3 -> (403., 410.)
  | 3, 3 -> (490., 410.)
  | _ -> failwith "Invalid coordinate"

(* Audio *)
let block_move_sound = Raylib.load_music_stream "resources/block.mp3"
let button_click_sound = Raylib.load_music_stream "resources/button.mp3"
