let write_to_file filename content =
  let channel = open_out filename in
  output_string channel content;
  flush channel;
  close_out channel
