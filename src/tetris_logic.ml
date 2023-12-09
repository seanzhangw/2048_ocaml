open Block
open Block_logic
open Constants

let generate_block =
  let col = Random.int num_squares in
  Block.place_block (random_mag ()) (0, col)
