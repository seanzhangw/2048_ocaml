(* Screen dimensions & display *)
let screen_width = 800
let screen_height = 600
let num_squares = 4
let square_size = 80
let spacing = 7
let fps = 60
let home_pos_x = 37
let home_width = 184
let home_pos_y = 30
let home_height = 56
let home_text_size = 80
let new_pos_x = 615
let new_width = 150
let new_pos_y = 37
let new_height = 58
let new_text_size = 20
let score_label_pos_x = 300
let score_label_pos_y = 37
let score_label_size = 15
let score_pos_x = 300
let score_pos_y = 57
let score_size = 47
let hs_label_pos_x = 450
let hs_label_pos_y = 37
let hs_label_size = 15
let hs_pos_x = 450
let hs_pos_y = 57
let hs_size = 47
let encouragement_text_pos_x = 60
let encouragement_text_pos_y = 100
let encouragement_text_size = 50

let color_mapping = function
  | 2 -> Raylib.Color.skyblue
  | 4 -> Raylib.Color.blue
  | 8 -> Raylib.Color.darkblue
  | 16 -> Raylib.Color.darkpurple
  | 32 -> Raylib.Color.purple
  | 64 -> Raylib.Color.violet
  | 128 -> Raylib.Color.maroon
  | 256 -> Raylib.Color.magenta
  | 512 -> Raylib.Color.pink
  | 1024 -> Raylib.Color.orange
  | 2048 -> Raylib.Color.red
  | _ -> Raylib.Color.beige

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
let initial_block_factor = 0.3
let block_scaling = 5.0

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
  | _ -> failwith "Invalid pos coordinate"

let block_board_mapping = function
  | 229., 169. -> (0, 0)
  | 316., 169. -> (1, 0)
  | 403., 169. -> (2, 0)
  | 490., 169. -> (3, 0)
  | 229., 256. -> (0, 1)
  | 316., 256. -> (1, 1)
  | 403., 256. -> (2, 1)
  | 490., 256. -> (3, 1)
  | 229., 343. -> (0, 2)
  | 316., 343. -> (1, 2)
  | 403., 343. -> (2, 2)
  | 490., 343. -> (3, 2)
  | 229., 430. -> (0, 3)
  | 316., 430. -> (1, 3)
  | 403., 430. -> (2, 3)
  | 490., 430. -> (3, 3)
  | _ -> failwith "Invalid list coordinate"
