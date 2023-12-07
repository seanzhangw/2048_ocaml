open Unix

(* Function to create a directory *)
let create_directory dir_name =
  try mkdir dir_name 0o755 with
  | Unix_error (EEXIST, _, _) -> ()
  | e -> raise e

let write_to_file filename content =
  create_directory "data";
  let channel = open_out filename in
  output_string channel content;
  flush channel;
  close_out channel

let read_highscore filename =
  create_directory "data";
  try
    let ic = open_in filename in
    try
      let line = input_line ic in
      close_in ic;
      int_of_string line
    with e ->
      close_in_noerr ic;
      0
  with e ->
    create_directory "data";
    write_to_file filename "0";
    0
