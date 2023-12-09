(** Utilities for file and directory operations. *)

val create_directory : string -> unit
(** [create_directory dir_name] creates a new directory with the given name
    [dir_name]. If the directory already exists, it does nothing. It raises
    exceptions for errors other than an already existing directory. *)

val write_to_file : string -> string -> unit
(** [write_to_file filename content] writes the [content] string to the file
    specified by [filename]. If the file doesn't exist, it's created. If the
    directory for the file doesn't exist, it's created. The content is written
    to "data" directory. *)

val read_highscore : string -> int
(** [read_highscore filename] reads the high score from the file specified by
    [filename]. If the file doesn't exist or an error occurs during reading, it
    returns 0 and ensures a file with a high score of 0 is created. The high
    score is read from "data" directory. *)
