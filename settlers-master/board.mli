(** Generator for the ASCII-art Game Board *)

(** [print_board width height st tiles] prints the board associated with
    int [width], int [height], state [st], and tiles [tiles]. *)
val print_board : int -> int -> State.t -> int option -> unit

(** [get_hex_vertices tile] is the list of tuples representing the vertices
    of tile [tile]. *)
val get_hex_vertices : State.tile -> (int * int) list

(** [compute_vertex_list acc_lst tiles] is the list of lists of vertices
    associated with tile list [tiles]. *)
val compute_vertex_list : (int * int) list list -> State.tile list -> 
  (int * int) list list

(** [hex_width] is a constant width of the hex on the tile. *)
val hex_width : int

(** [hex_height] is a constant width of the hex on the tile. *)
val hex_height : int

(** [are_verts_equal vert1 vert2] is true if vert1 and vert2 are equal and 
    false otherwise. Equal here is defined as having both coordinates being
    equal. *)
val are_verts_equal : ('a * 'b) -> ('a * 'b) -> bool

(** [is_in_list elt lst] is true if elt is in lst and false otherwise. *)
val is_in_list : ('a * 'b) -> ('a * 'b) list -> bool

(** [check_for_vert_in_lists vert lst_of_lsts] checks if vert is in all of the
    lists in lst_of_lsts. *)
val check_for_vert_in_lists : ('a * 'b) -> ('a * 'b) list list -> bool

(** [print_res st] prints a given player's resources on the board on that
    player's turn. *)
val print_res : State.t -> unit