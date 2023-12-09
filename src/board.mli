(** This file handles functions to draw the 2048 game interface, including the
    initial grid, home page button, new game button, individual blocks, and the
    main game page *)

type block = Block.Block.block
(** type [block] represents a tile in the grid. *)

val draw_init_grid : unit -> unit
(** [draw_init_grid ()] draws the initial grid for the game board. *)

val draw_home_page_button : unit -> unit
(** [draw_home_page_button ()] draws the home page button with the game title. *)

val draw_new_game_button : unit -> unit
(** [draw_new_game_button ()] draws the new game button with hover effects. *)

val draw_block : block -> int -> unit
(** [draw_block block size] draws a single block on the game board with its
    numeric value. *)

val display_tiles_input : block list list -> unit
(** [display_tiles_input tiles] displays the current state of the game board. *)

val game_page : unit -> unit
(** [game_page ()] draws the main content of the game page, including title and
    buttons. *)
