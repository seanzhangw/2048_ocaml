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
let move_cooldown = 0.15
let block_speed = 2.0
let block_scaling = 2.0

let block_position_mapping = function
  | 0, 0 -> (229., 169.)
  | 1, 0 -> (316., 169.)
  | 2, 0 -> (403., 169.)
  | 3, 0 -> (490., 169.)
  | 0, 1 -> (229., 256.)
  | 1, 1 -> (316., 256.)
  | 2, 1 -> (403., 256.)
  | 3, 1 -> (490., 256.)
  | 0, 2 -> (229., 343.)
  | 1, 2 -> (316., 343.)
  | 2, 2 -> (403., 343.)
  | 3, 2 -> (490., 343.)
  | 0, 3 -> (229., 430.)
  | 1, 3 -> (316., 430.)
  | 2, 3 -> (403., 430.)
  | 3, 3 -> (490., 430.)
  | _ -> failwith "Invalid coordinate"
