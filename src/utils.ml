let write_to_file filename content =
  let channel = open_out filename in
  output_string channel content;
  flush channel;
  close_out channel

let read_highscore filename =
  let ic = open_in filename in
  try
    let line = input_line ic in
    close_in ic;
    int_of_string line
  with e ->
    close_in_noerr ic;
    0
