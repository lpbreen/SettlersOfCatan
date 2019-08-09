open ANSITerminal
open State

(** tile_color_type is a type representing whether an ANSITerminal style of
    type Foreground of Color or of type Background of Color is desired. *)
type tile_color_type = Text | Background

(** Constants *)
let hex_width = 11
let hex_height = 6

(** [tiles] represents the list of tiles to be drawn. *)
(* let tiles = State.get_tiles State.init *)

(** [get_tile_color tile_type color_type] is the ANSITerminal style associated
    with State.resource [tile_type]. If tile_color_type [color_type] is Text,
    this returns a Foreground style, otherwise it returns a Background style. 
*)
let get_tile_color (tile_type : State.resource) (color_type : tile_color_type) 
  =
  match tile_type with
  | Brick -> if color_type = Text
    then [Foreground Red] else [Background Red]
  | Wood -> if color_type = Text
    then [Foreground Green] else [Background Green]
  | Sheep -> if color_type = Text
    then [Foreground Green] else [Background Green]
  | Grain -> if color_type = Text
    then [Foreground Yellow] else [Background Yellow]
  | Rock -> if color_type = Text
    then [Foreground Default] else [Background Default]
  | Desert -> if color_type = Text
    then [Foreground Yellow] else [Background Yellow]
  | Ocean -> if color_type = Text
    then [Foreground Blue] else [Background Blue]

(** [get_tile_str tile_type] is the string corresponding to a State.resource.
    The strings must be obtained for printing purposes. *)
let get_tile_str (tile_type : State.resource) =
  match tile_type with
  | Brick -> "Brick"
  | Wood -> "Wood"
  | Sheep -> "Sheep"
  | Grain -> "Grain"
  | Rock -> "Rock"
  | Desert -> "Desert"
  | Ocean -> "Ocean"

(** [get_tile_col_from_str str] is the color corresponding to a string that
    corresponds to a resource type. *)
let get_tile_col_from_str (str : string) =
  match str with
  | "Brick" -> get_tile_color Brick Text
  | "Wood" -> get_tile_color Wood Text
  | "Sheep" -> get_tile_color Sheep Text
  | "Grain" -> get_tile_color Grain Text
  | "Rock" -> get_tile_color Rock Text
  | "Desert" -> get_tile_color Desert Text
  | "Ocean" -> get_tile_color Ocean Text
  | _ -> failwith "Invalid resource type"

(** [get_style_from_col color] is the ANSITerminal Foreground of Color
    corresponding to State.color [color]. *)
let get_style_from_col (color : State.color) (color_type : tile_color_type) =
  match color with
  | Black -> if color_type = Text
    then Foreground Default else Background Default
  | Red -> if color_type = Text
    then Foreground Red else Background Red
  | Blue -> if color_type = Text
    then Foreground Blue else Background Blue
  | Green -> if color_type = Text
    then Foreground Green else Background Green
  | Yellow -> if color_type = Text
    then Foreground Yellow else Background Yellow
  | Cyan -> if color_type = Text
    then Foreground Cyan else Background Cyan
  | Magenta -> if color_type = Text
    then Foreground Magenta else Background Magenta
  | White -> if color_type = Text
    then Foreground White else Background White

(** [draw_name tile blinking] adds tile [tile]'s name to the board. A tile's 
    name is its resource type. If [blinking] is true, then the name will be 
    blinking. *)
let draw_name tile blinking =
  let str = get_tile_str tile.resource in
  ANSITerminal.move_cursor (-10) (0);
  if blinking then
    print_string (Blink::(get_tile_color tile.resource Text)) str
  else print_string (get_tile_color tile.resource Text) str;
  ANSITerminal.move_cursor (10 - String.length str) (0)

(** [draw_die_num tile] adds tile [tile]'s die number to the board. *)
let draw_die_num tile =
  let num = ("Roll "^(string_of_int tile.dienum)) in
  ANSITerminal.move_cursor (-10) (0);
  print_string [default] num;
  ANSITerminal.move_cursor (10 - String.length num) (0)

(** [draw_grid_num tile] adds tile [tile]'s grid number to the board. *)
let draw_grid_num tile =
  let num = string_of_int tile.gridnum in
  ANSITerminal.move_cursor (-7) (0);
  print_string [default] num;
  ANSITerminal.move_cursor (7 - String.length num) (0)

(** [draw_robber tile blinking] draws the robber on tile [tile]. *)
let draw_robber tile blinking =
  if not(blinking) then
    let str = "Robber" in
    ANSITerminal.move_cursor (-10) (0);
    print_string [Bold;Underlined] str;
    ANSITerminal.move_cursor (10 - String.length str) (0) else
    let str = "Robber" in
    ANSITerminal.move_cursor (-10) (0);
    print_string [Blink;Bold;Underlined] str;
    ANSITerminal.move_cursor (10 - String.length str) (0)

(** [draw_port tile] draws port information on tile [tile]. *)
let draw_port tile =
  if tile.dienum = 21 then let str = (get_tile_str tile.resource)^" Port" in
    ANSITerminal.move_cursor (-13) (0);
    print_string (Bold::(get_tile_color tile.resource Text)) str;
    ANSITerminal.move_cursor (13 - String.length str) (0)
  else let str = "Port" in
    ANSITerminal.move_cursor (-10) (0);
    print_string (Bold::(get_tile_color tile.resource Text)) str;
    ANSITerminal.move_cursor (10 - String.length str) (0)

(** [draw_port_conversion tile] draws the conversion information on tile
    [tile]. *)
let draw_port_conversion tile =
  if tile.dienum = 21 then (
    ANSITerminal.move_cursor (-10) (0);
    print_string [default] "2 : 1";
    ANSITerminal.move_cursor (10 - String.length "2 : 1") (0))
  else if tile.dienum = 31 then (
    ANSITerminal.move_cursor (-10) (0);
    print_string [default] "3 : 1";
    ANSITerminal.move_cursor (10 - String.length "3 : 1") (0))

(** [draw_hex x y row col_mult tiles] draws the hex that represents a tile in
    tile list [tiles]. The hex drawn is determined by row [row] and column
    multiplier [col_mult]. It's top-left vertex is placed at (x,y). *)
let draw_hex x y row col_mult tiles num_opt st =
  let tile = List.nth tiles (row+(7)*col_mult) in
  match tile.resource with
  | Ocean -> if (not(tile.dienum = 21 || tile.dienum = 31)) then (
      ANSITerminal.set_cursor x y;
      print_string [] "o______o";
      ANSITerminal.move_cursor (-9) (1);
      print_string [] "/";
      print_string (get_tile_color tile.resource Text) "........";
      print_string [] "\\";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "/";
      print_string (get_tile_color tile.resource Text) "..........";
      print_string [] "\\";
      draw_grid_num tile;
      ANSITerminal.move_cursor (-14) (1);
      print_string [] "o/";
      print_string (get_tile_color tile.resource Text) "............";
      print_string [] "\\o";
      draw_name tile false;
      ANSITerminal.move_cursor (-15) (1);
      print_string [] "\\";
      print_string (get_tile_color tile.resource Text) "............";
      print_string [] "/";
      ANSITerminal.move_cursor (-13) (1);
      print_string [] "\\";
      print_string (get_tile_color tile.resource Text) "..........";
      print_string [] "/";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "\\o______o/" )
    else (
      ANSITerminal.set_cursor x y;
      print_string [] "o______o";
      ANSITerminal.move_cursor (-9) (1);
      print_string [] "/";
      print_string [Foreground Blue] "........";
      print_string [] "\\";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "/";
      print_string [Foreground Blue] "..........";
      print_string [] "\\";
      draw_grid_num tile;
      ANSITerminal.move_cursor (-14) (1);
      print_string [] "o/";
      print_string [Foreground Blue] "............";
      print_string [] "\\o";
      draw_port tile;
      ANSITerminal.move_cursor (-15) (1);
      print_string [] "\\";
      print_string [Foreground Blue] "............";
      print_string [] "/";
      ANSITerminal.move_cursor (-13) (1);
      print_string [] "\\";
      print_string [Foreground Blue] "..........";
      print_string [] "/";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "\\o______o/"; )
  | _ -> if (not(tile.dienum = 21 || tile.dienum = 31))
    then (
      ANSITerminal.set_cursor x y;
      print_string [default] "o______o";
      ANSITerminal.move_cursor (-9) (1);
      print_string [default] ("/        \\");
      ANSITerminal.move_cursor (-11) (1);
      print_string [default] ("/          \\");
      draw_grid_num tile;
      ANSITerminal.move_cursor (-14) (1);
      print_string [default] "o/            \\o";
      (match num_opt with
       | Some num -> if tile.dienum = num && tile.dienum <> 7 then
           (if not(tile.gridnum = State.get_robber_tile st)
            then draw_name tile true else draw_name tile false) else
           draw_name tile false
       | None -> draw_name tile false);
      ANSITerminal.move_cursor (-15) (1);
      print_string [default] "\\            /";
      if (not(tile.gridnum = State.get_robber_tile st)) then
        (if not(tile.dienum = 7) then draw_die_num tile else ()) else (
        match num_opt with
        | Some num -> if (num = 7 || tile.dienum = num)
          then draw_robber tile true
          else draw_robber tile false
        | None -> draw_robber tile false
      );
      ANSITerminal.move_cursor (-13) (1);
      print_string [default] "\\          /";
      ANSITerminal.move_cursor (-11) (1);
      print_string [default] "\\o______o/")
    else (
      ANSITerminal.set_cursor x y;
      print_string [] "o______o";
      ANSITerminal.move_cursor (-9) (1);
      print_string [] "/";
      print_string [Foreground Blue] "........";
      print_string [] "\\";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "/";
      print_string [Foreground Blue] "..........";
      print_string [] "\\";
      draw_grid_num tile;
      ANSITerminal.move_cursor (-14) (1);
      print_string [] "o/";
      print_string [Foreground Blue] "............";
      print_string [] "\\o";
      draw_port tile;
      ANSITerminal.move_cursor (-15) (1);
      print_string [] "\\";
      print_string [Foreground Blue] "............";
      print_string [] "/";
      ANSITerminal.move_cursor (-13) (1);
      print_string [] "\\";
      print_string [Foreground Blue] "..........";
      print_string [] "/";
      ANSITerminal.move_cursor (-11) (1);
      print_string [] "\\o______o/";
    )

(** [draw_hex_col tiles x y num acc col_mult num_opt st] draws a column of
    hexes. *)
let rec draw_hex_col tiles x y num acc col_mult num_opt st =
  if acc > num-1
  then () else (draw_hex x y acc col_mult tiles num_opt st;
                let (x',y') = (x,y+hex_height) in
                draw_hex_col tiles x' y' num (acc+1) col_mult num_opt st)

(** [draw_all_cols tile_lst x y width height col_acc num_opt st] prints all 
    the
    board columns on the board. *)
let rec draw_all_cols tile_lst x y width height col_acc num_opt st =
  if col_acc > width-1
  then ()
  else (draw_hex_col tile_lst x y height 0 (col_acc) num_opt st;
        if col_acc mod 2 = 0
        then (let (x',y') = (x+hex_width,y-hex_height/2) in
              draw_all_cols tile_lst x' y' width height (col_acc+1) num_opt st)
        else (let (x',y') = (x+hex_width,y+hex_height/2) in
              draw_all_cols tile_lst x' y' width height (col_acc+1) num_opt st))

(** [get_col tile] is the column associated with tile [tile]. *)
let get_col tile =
  tile.gridnum/7

(** [get_hex_vertices tile] is the vertex list associated with tile [tile]. *)
let rec get_hex_vertices tile =
  let col = get_col tile in
  if col mod 2 = 0 then [(60+hex_width*col,10+hex_height*(tile.gridnum mod 7));
                         (67+hex_width*col,10+hex_height*(tile.gridnum mod 7));
                         (71+hex_width*col,13+hex_height*(tile.gridnum mod 7));
                         (67+hex_width*col,16+hex_height*(tile.gridnum mod 7));
                         (60+hex_width*col,16+hex_height*(tile.gridnum mod 7));
                         (56+hex_width*col,13+hex_height*(tile.gridnum mod 7))]
  else [(60+hex_width*col,7+hex_height*(tile.gridnum mod 7));
        (67+hex_width*col,7+hex_height*(tile.gridnum mod 7));
        (71+hex_width*col,10+hex_height*(tile.gridnum mod 7));
        (67+hex_width*col,13+hex_height*(tile.gridnum mod 7));
        (60+hex_width*col,13+hex_height*(tile.gridnum mod 7));
        (56+hex_width*col,10+hex_height*(tile.gridnum mod 7))]

(** [compute_vertex_list acc_lst tiles] is the list of vertex lists associated
    with each tile in [tiles]. *)
let rec compute_vertex_list acc_lst tiles =
  match tiles with
  | [] -> List.rev acc_lst
  | h::t -> compute_vertex_list ((get_hex_vertices h)::acc_lst) t


let rec are_verts_equal vert1 vert2 =
  fst vert1 = fst vert2 && snd vert1 = snd vert2

(** [is_in_list elt lst] is true if elt is in lst and false otherwise. *)
let rec is_in_list elt lst =
  match lst with
  | [] -> false
  | h::t -> (are_verts_equal elt h) || is_in_list elt t

(** [check_for_vert_in_lists vert lst_of_lsts] checks if vert is in all of the
    lists in lst_of_lsts. *)
let rec check_for_vert_in_lists vert lst_of_lsts =
  match lst_of_lsts with
  | [] -> true
  | h::t -> (is_in_list vert h) && (check_for_vert_in_lists vert t)

(** [get_vert vert_lists] returns the common vertex to all the lists in
    'a * 'b list list [vert_lists]. *)
let rec get_vert vert_lists =
  match vert_lists with
  | h1::h2::h3::[] -> (match h1 with
      | [] -> failwith "No common vertex"
      | h'::t' -> if (check_for_vert_in_lists h' (h2::h3::[]))
        then h' else (get_vert (t'::h2::h3::[])))
  | _ -> failwith "get_vert failed"

(** [get_road_verts vert_lists acc_lst] is the list of vertices that the vertex
    lists [vert_lists] have in common. The list will have no more than two 
    elements due to the nature of the hex setup. *)
let rec get_road_verts vert_lists acc_lst =
  match vert_lists with
  | h1::h2::[] -> (match h1 with
      | [] -> acc_lst
      | h'::t' -> if (check_for_vert_in_lists h' ([h2]))
        then get_road_verts (t'::h2::[]) (h'::acc_lst)
        else get_road_verts (t'::h2::[]) (acc_lst))
  | _ -> failwith "get_road_verts failed"

(** [print_grid_numes tiles] prints the grid numbers of tiles in tile list
    [tiles]. *)
let rec print_grid_nums (tiles : State.location) =
  match tiles with
  | [] -> ()
  | h::t -> print_int h.gridnum; (print_grid_nums t)

(** [print_road verts color] prints a road with vertices [verts] and color
    [color]. *)
let print_road verts color =
  match verts with
  | h1::h2::[] ->
    (if (fst h1) + 7 = (fst h2) && (snd h1) = (snd h2) then (
        ANSITerminal.set_cursor (fst h1 + 1) (snd h1);
        print_string [Bold;(get_style_from_col color Text)] "______")
     else if (fst h1) - 7 = (fst h2) && (snd h1) = (snd h2) then (
       ANSITerminal.set_cursor (fst h2 + 1) (snd h1);
       print_string [Bold;(get_style_from_col color Text)] "______")
     else if (fst h1) + 4 = (fst h2) && (snd h1) - 3 = (snd h2) then (
       ANSITerminal.set_cursor (fst h1) (snd h1);
       ANSITerminal.move_cursor (1) (0);
       print_string [Bold;(get_style_from_col color Text)] "/";
       ANSITerminal.move_cursor (0) (-1);
       print_string [Bold;(get_style_from_col color Text)] "/";
       ANSITerminal.move_cursor (0) (-1);
       print_string [Bold;(get_style_from_col color Text)] "/";
     )
     else if (fst h1) - 4 = (fst h2) && (snd h1) + 3 = (snd h2) then (
       ANSITerminal.set_cursor (fst h2) (snd h2);
       ANSITerminal.move_cursor (1) (0);
       print_string [Bold;(get_style_from_col color Text)] "/";
       ANSITerminal.move_cursor (0) (-1);
       print_string [Bold;(get_style_from_col color Text)] "/";
       ANSITerminal.move_cursor (0) (-1);
       print_string [Bold;(get_style_from_col color Text)] "/";
     )
     else if (fst h1) + 4 = (fst h2) && (snd h1) + 3 = (snd h2) then (
       ANSITerminal.set_cursor (fst h1) (snd h1);
       ANSITerminal.move_cursor (1) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
       ANSITerminal.move_cursor (0) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
       ANSITerminal.move_cursor (0) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
     )
     else if (fst h1) - 4 = (fst h2) && (snd h1) - 3 = (snd h2) then (
       ANSITerminal.set_cursor (fst h2) (snd h2);
       ANSITerminal.move_cursor (1) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
       ANSITerminal.move_cursor (0) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
       ANSITerminal.move_cursor (0) (1);
       print_string [Bold;(get_style_from_col color Text)] "\\";
     )
    )

  | _ -> failwith "Invalid road"

(** [print_all place_lst height st] prints all the placements to the board,
    including ports. *)
let rec print_all (place_lst : placement list) height st = 
  match place_lst with
  | [] -> ()
  | h::t -> (match h with
      | Building b ->
        (match b.build_type with
         | Settlement
         | City ->
           let (x,y) = get_vert (compute_vertex_list [] b.location) in
           ANSITerminal.set_cursor x y;
           if b.build_type = Settlement
           then print_string
               [Bold;(get_style_from_col b.player.color Text)] "S"
           else print_string
               [Bold;(get_style_from_col b.player.color Text)] "C";
           ANSITerminal.set_cursor 0 ((height+2)*hex_height);
           print_all t height st
         | Road -> (match (get_road_verts
                             (compute_vertex_list [] b.location) []) with
                   | [(x1,y1);(x2,y2)] ->
                     print_road [(x1,y1);(x2,y2)] b.player.color;
                     ANSITerminal.set_cursor 0 ((height+2)*hex_height);
                     print_all t height st
                   | _ -> failwith "Road vertices incorrect")
         | _ -> failwith "Not yet implemented")
      | Port (res, loc) ->
        (match get_vert (compute_vertex_list [] loc) with
         | (x,y) ->
           ANSITerminal.set_cursor x y;
           print_string [default] "P";
           ANSITerminal.set_cursor 0 ((height+2)*hex_height);
           print_all t height st)
    )

(** [print_placements place_lst height st] prints all the placements on the
    board to the terminal (including settlements on ports). *)
let rec print_placements (place_lst : placement list) height st =
  match place_lst with
  | [] -> ()
  | h::t -> (match h with
      | Building b ->
        (match b.build_type with
         | Settlement
         | City ->
           let (x,y) = get_vert (compute_vertex_list [] b.location) in
           ANSITerminal.set_cursor x y;
           if b.build_type = Settlement
           then print_string [Bold;
                              (get_style_from_col b.player.color Text)] "S"
           else print_string [Bold;
                              (get_style_from_col b.player.color Text)] "C";
           ANSITerminal.set_cursor 0 ((height+2)*hex_height);
           print_placements t height st
         | Road -> (match (get_road_verts
                             (compute_vertex_list [] b.location) []) with
                   | [(x1,y1);(x2,y2)] ->
                     print_road [(x1,y1);(x2,y2)] b.player.color;
                     ANSITerminal.set_cursor 0 ((height+2)*hex_height);
                     print_placements t height st
                   | _ -> failwith "Road vertices incorrect")
         | _ -> failwith "Not yet implemented")
      | Port (res, loc) ->
        print_placements t height st
    )

(** [print_each_card card_lst] loops through card list and prints each card
    inside it nicely. *)
let rec print_each_card card_lst =
  match card_lst with
  | [] -> ()
  | h::t ->
    let str = State.card_to_string h in
    print_string [default] str;
    ANSITerminal.move_cursor (-(String.length str)) (1);
    print_each_card t

(** [print_cards st] prints the list of cards a player has in addition to the
    formatting strings. *)
let print_cards st =
  let init_x = 10 in
  let init_y = 30 in
  ANSITerminal.set_cursor init_x init_y;
  print_string [default] "Your Cards:";
  ANSITerminal.set_cursor init_x (init_y+2);
  print_each_card (get_current_player_cards st)

let print_res st =
  let init_x = 20 in
  let init_y = 15 in

  ANSITerminal.set_cursor (init_x-2+4) (init_y-3);
  print_string (
    (get_style_from_col (
        (State.get_current_player st).color) Text)::[Underlined;Bold])
    (State.get_current_player st).name;
  ANSITerminal.set_cursor (init_x-2) (init_y-2);
  print_string [Bold] "____________";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-12) (1);
  print_string [Bold] "|          |";
  ANSITerminal.move_cursor (-11) (0);
  print_string [Bold] "__________";

  ANSITerminal.set_cursor (init_x) (init_y);
  let printer_func (a,b) =
    let str_len = String.length (a^": "^(string_of_int b)) in
    print_string (get_tile_col_from_str a) a; print_string [black] ": ";
    print_int b; ANSITerminal.move_cursor (-str_len) (1) in

  List.iter printer_func (State.resources st)

let print_board width height st num =
  let tiles = State.get_tiles st in
  let (start_x,start_y) = ANSITerminal.pos_cursor () in
  if State.get_turn_status st then print_res st;
  if State.get_turn_status st then print_cards st;
  ANSITerminal.set_cursor start_x start_y;
  (* draw_hex 10 10; *)
  (* ignore(draw_hex_col 10 10 1 height); *)
  ignore(draw_all_cols tiles 60 10 width height 0 num st);
  print_all (State.get_placements st) height st;
  print_placements (State.get_placements st) height st;
  print_endline "";
  print_endline ""
