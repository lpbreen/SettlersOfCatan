(** Representation of the Game states. *)

type expansion = Basic

type resource = Brick | Wood | Sheep | Grain | Rock | Desert | Ocean

type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

type progress = YearOfPlenty | Monopoly | RoadBuilding

type card = Knight of {played : bool}
          | Progress of {played : bool;
                         progress : progress}
          | VP of {played : bool; pair : string * int}

type player = {name:string; score:int; color:color;
               resources: (resource * int) list; cards: card list; is_ai: bool}

type tile = {resource:resource; dienum:int; gridnum:int}

type location = tile list

type build_type = Settlement | Road | City | Ship | Wall

type building = {player:player; location:location; build_type:build_type}

type placement = Building of building | Port of resource * location

type t =
  {players: player list; tiles: tile list; buildings: placement list;
   current_player: player; player_order: (int * player) list; turn:int;
   dimensions: int * int; start:bool; roll_tiles: (int * tile list) list;
   expansion: expansion; deck: card list; robber: int;
   longest_road: int; has_drawn : bool; has_used_card: bool }

type prompt = Discard of (player * int) list * t
            | Trade of player * (resource * int) list * (resource * int) list
                       * t

type result = Legal of t | Illegal | Prompt of prompt | Winner of player

let get_turn_status st = st.start

let get_buildings_of_player st pname =
  let rec get_buildings_help placements acc = match placements with
    | [] -> acc
    | h::t -> match h with
      | Building building ->
        if building.player.name = pname then
          get_buildings_help t (building::acc)
        else
          get_buildings_help t acc
      | Port _ -> get_buildings_help t acc
  in
  get_buildings_help st.buildings []

let get_tiles st = st.tiles

let get_robber_tile st = st.robber

let get_dimensions st =
  st.dimensions

let get_placements st =
  st.buildings

(** [get_buildings st] extracts the buildings from the placement list in [st]*)
let get_buildings st =
  let rec build_from_placement lst acc = match lst with
    | [] -> acc
    | h::t -> begin
        match h with
        | Building b -> build_from_placement t (b::acc)
        | Port _ -> build_from_placement t acc
      end
  in
  build_from_placement st.buildings []

let get_players st = st.players

let get_current_player st =
  st.current_player

let get_turn_status st =
  st.start

let get_ports st =
  let rec ports_from_placement lst acc = match lst with
    | [] -> acc
    | h::t -> (match h with
        | Building _ -> ports_from_placement t acc
        | Port (_,_) as p -> ports_from_placement t (p::acc))
  in
  ports_from_placement st.buildings []

(** [get_roll_tiles tiles] is the association list mapping integers for dice
    rolls with the tiles they correspond to. *)
let get_roll_tiles tiles =
  let rec has_roll lst acc d = match lst with
    | [] -> acc
    | h::t -> if h.dienum = d then has_roll t (h::acc) d else has_roll t acc d
  in
  let rec to_twelve n f acc = if n < 13 then to_twelve (n+1) f ((n,f n)::acc)
    else acc
  in
  let f = has_roll tiles []  in
  to_twelve 2 f []

(** [dice_roll ()] is an int between 2 and 12, generated pseudorandomly. *)
let dice_roll () =
  let die = QCheck.Gen.int_range 1 6 in
  (QCheck.Gen.generate1 die) + (QCheck.Gen.generate1 die)

let sort_by_gridnum tile_list =
  List.sort (fun x y -> if x.gridnum < y.gridnum then -1 else if
                x.gridnum = y.gridnum then 0 else 1 ) tile_list

(** This block of code is used to generate random board configurations. *)

let basic_tile_lst = [Desert;
                      Brick;Brick;Brick;
                      Rock;Rock;Rock;
                      Sheep;Sheep;Sheep;Sheep;
                      Wood;Wood;Wood;Wood;
                      Grain;Grain;Grain;Grain]

let num_basic_tiles = 19

(* Want to generate assignment from basic_tile_lst that hasn't been assigned
   yet to one of the randomizable gridnums, repeat that for port tiles, then
   add those tiles to the ocean tiles. Don't currently know algorithm for which
   tiles should be which, but we can hard code the values for basic bc won't
   change *)

let rec assign_resources_to_tiles gridnum_list acc tiles =
  match gridnum_list with
  | [] -> acc
  | h::t ->
    assign_resources_to_tiles t ((h, List.hd tiles)::acc) (List.tl tiles)

(** [filter_tile_lst lst resource acc] returns a new list
    with the specified resource tile removed. *)
let rec filter_tile_lst lst resource acc=
  match lst with
  | [] -> acc
  | h::t -> if resource = h then List.rev_append (List.rev acc) t
    else (filter_tile_lst t resource (h::acc))

let order_numbers = [5;2;6;3;8;10;9;12;11;4;8;10;9;4;5;6;3;11]

let outer_tile_coordinates = [9;10;11;18;26;32;39;38;29;22;15]

(** [gen_die_roll_list n] returns an ordered list of die placement
    before the desert tile depending on the outer tile chosen. *)
let rec gen_die_roll_list n =
  if n = 9 then [(9, 5);(10, 2);(11, 6);(18,3);(26,8);(32,10);(39,9);(38,12);
                 (37,11);(29,4);(22,8);(15,10);(16,9);(17,4);(25,5);
                 (31,6);(30,3);(23,11);(24,0);]
  else if n = 10 then [(10, 5);(11, 2);(18, 6);(26,3);(32,8);(39,10);(38,9);
                       (37,12);(29,11);(22,4);(15,8);(9,10);(16,9);(17,4);
                       (25,5);(31,6);(30,3);(23,11);(24,0);]
  else if n = 11 then [(11, 5);(18, 2);(26, 6);(32,3);(39,8);(38,10);
                       (37,9);(29,12);(22,11);(15,4);(9,8);(10,10);
                       (17,9);(25,4);(31,5);(30,6);(23,3);(16,11);(24,0);]
  else if n = 18 then [(18, 5);(26, 2);(32, 6);(39,3);(38,8);(37,10);
                       (29,9);(22,12);(15,11);(9,4);(10,8);(11,10);
                       (17,9);(25,4);(31,5);(30,6);(23,3);(16,11);(24,0);]
  else if n = 26 then [(26, 5);(32, 2);(39, 6);(38,3);(37,8);
                       (29,10);(22,9);(15,12);(9,11);(10,4);
                       (11,8);(18,10);(25,9);(31,4);(30,5);
                       (23,6);(16,3);(17,11);(24,0);]
  else if n = 32 then [(32, 5);(39, 2);(38, 6);(37,3);(29,8);
                       (22,10);(15,9);(9,12);(10,11);(11,4);
                       (18,8);(26,10);(25,9);(31,4);(30,5);
                       (23,6);(16,3);(17,11);(24,0);]
  else if n = 39 then [(39, 5);(38, 2);(37, 6);(29,3);(22,8);
                       (15,10);(9,9);(10,12);(11,11);(18,4);
                       (26,8);(32,10);(31,9);(30,4);(23,5);
                       (16,6);(17,3);(25,11);(24,0);]
  else if n = 38 then [(38, 5);(37, 2);(29, 6);(22,3);
                       (15,8);(9,10);(10,9);(11,12);(18,11);(26,4);
                       (32,8);(39,10);(31,9);(30,4);(23,5);(16,6);
                       (17,3);(25,11);(24,0);]
  else if n = 37 then [(37, 5);(29, 2);(22, 6);(15,3);(9,8);(10,10);
                       (11,9);(18,12);(26,11);(32,4);(39,8);(38,10);
                       (30,9);(23,4);(16,5);(17,6);(25,3);(31,11);(24,0);]
  else if n = 29 then [(29, 5);(22, 2);(15, 6);(9,3);(10,8);(11,10);
                       (18,9);(26,12);(32,11);(39,4);(38,8);(37,10);
                       (30,9);(23,4);(16,5);(17,6);(25,3);(31,11);(24,0);]
  else if n = 22 then [(22, 5);(15, 2);(9, 6);(10,3);(11,8);(18,10);
                       (26,9);(32,12);(39,11);(38,4);(37,8);(29,10);
                       (23,9);(16,4);(17,5);(25,6);(31,3);(30,11);(24,0);]
  else [(15, 5);(9, 2);(10, 6);(11,3);(18,8);(26,10);(32,9);(39,12);
        (38,11);(37,4);(29,8);(22,10);(23,9);(16,4);(17,5);(25,6);
        (31,3);(30,11);(24,0);]

(** [shift lst acc num] returns a shifted die order placement depending
    on when the desert tile is found. *)
let rec shift lst acc num =
  match lst with
  | [] -> List.rev acc
  | (n,x)::t -> (shift (t) ((n,num)::acc) (x))

(** [lookup k] returns a die placement depending on the gridnum number. *)
let rec lookup k = function
  | [] -> 0
  | (k',v)::t -> if k=k' then v else lookup k t

(** [construct_basic_random_board acc acc_list tile_lst] returns a randomly
    ordered board with resources before the die order is placed. *)
let rec construct_basic_random_board acc acc_list tile_lst =
  if acc = 49 then acc_list else
  if acc < 9 || (acc > 39 && acc < 49) ||
     (acc > 11 && acc < 15) || (acc > 18 && acc < 22) ||
     (acc > 26 && acc < 29) || (acc > 32 && acc < 37)
  then let gen_resource =
         {resource = Ocean; dienum = 0; gridnum = acc} in
    construct_basic_random_board (acc + 1) (gen_resource::acc_list)
      (filter_tile_lst tile_lst gen_resource.resource [])
  else
    let random_element = List.nth tile_lst
        (Random.int (List.length tile_lst)) in
    if random_element <> Desert then
      let gen_resource =
        {resource = random_element; dienum = 99; gridnum = acc} in
      construct_basic_random_board (acc + 1)
        (gen_resource::acc_list)
        (filter_tile_lst tile_lst gen_resource.resource [])
    else
      let gen_resource = {resource = Desert; dienum = 7; gridnum = acc} in
      construct_basic_random_board (acc + 1)
        (gen_resource::acc_list)
        (filter_tile_lst tile_lst gen_resource.resource [])

(** [find_desert_tile tile_lst] returns the gridnum of the Desert tile. *)
let rec find_desert_tile tile_list =
  match tile_list with
  | [] -> 0
  | {resource = x; dienum = c; gridnum = v}::t ->
    if x = Desert then v else find_desert_tile t

(** [update_list_with_desert_tile acc_list lst desert_num]
    updates the die order placement with the specificied desert tile. *)
let rec update_list_with_desert_tile acc_list lst desert_num=
  match lst with
  | [] -> List.rev acc_list
  | (n,v)::t -> if n = desert_num then
      let num = v in
      let shifted_lst = shift t [] num in
      update_list_with_desert_tile acc_list shifted_lst desert_num
    else update_list_with_desert_tile ((n,v)::acc_list) t desert_num

(** [construct_board_with_dies tile_lst die_list acc_lst] returns
    a tile board with a specified die order placement. *)
let rec construct_board_with_dies tile_lst die_list acc_lst=
  match tile_lst with
  | [] -> acc_lst
  | {resource = x; dienum = c; gridnum = v}::t -> if c = 99 then
      let new_die = lookup v die_list in
      construct_board_with_dies t die_list
        ({resource = x; dienum = new_die; gridnum = v}::acc_lst)
    else construct_board_with_dies t die_list
        ({resource = x; dienum = c; gridnum = v}::acc_lst)

let port_tiles = [(Sheep,21);(Wood,21);(Brick,21);
                  (Rock,21);(Grain,21);(Ocean,31);(Ocean,31);(Ocean,31);
                  (Ocean,31);]

(** [filter_port lst num snd acc] returns a port list with the
    filtered port tile removed. *)
let rec filter_port lst num snd acc=
  match lst with
  | [] -> acc
  | (n,x)::t -> if num = n && snd = x then List.rev_append (List.rev acc) t
    else (filter_port t num snd ((n,x)::acc))

(** [add_ports_to_board tile_lst acc_lst port_list] returns a
    tile board with randomly placed generic and special ports on the board. *)
let rec add_ports_to_board tile_lst acc_lst port_list=
  match tile_lst with
  | [] -> acc_lst
  | {resource = x; dienum = c; gridnum = v}::t ->
    if v = 8 || v = 36 || v = 21 || v = 44 || v = 2
       || v = 4 || v = 46 || v = 19 || v = 33 then
      let rand_port_tile = List.nth port_list
          (Random.int (List.length port_list)) in
      add_ports_to_board t
        ({resource = fst rand_port_tile;
          dienum = snd rand_port_tile; gridnum = v}::acc_lst)
        (filter_port port_list (fst rand_port_tile) (snd rand_port_tile) [])
    else add_ports_to_board t
        ({resource = x; dienum = c; gridnum = v}::acc_lst) port_list

(** [gen_board game_type] returns a randomly gen
    erated board depending on the expansion game_type played. *)
let gen_board game_type =
  match game_type with
  | Basic ->
    let no_dice_tiles = (construct_basic_random_board 0 [] basic_tile_lst) in
    let num = find_desert_tile no_dice_tiles in
    let old_die_list = gen_die_roll_list
        (List.nth outer_tile_coordinates
           (Random.int (List.length outer_tile_coordinates))) in
    let d_list = update_list_with_desert_tile [] old_die_list num in
    let no_ports = construct_board_with_dies (no_dice_tiles) d_list [] in
    add_ports_to_board (no_ports) [] port_tiles

(** [get_tile_loc tile_lst num] returns a resource depend
    ing on the specified gridnum. *)
let rec get_tile_loc tile_lst num=
  match tile_lst with
  | [] -> {resource = Ocean; dienum = 0; gridnum = 0}
  | {resource = x; dienum = c; gridnum = v}::t -> if v = num then
      {resource = x; dienum = c; gridnum = v} else get_tile_loc t num

(** [gen_port_location_list tile_lst v] returns a port location
    depending on the location of the port tile. *)
let gen_port_location_list tile_lst v =
  if v = 8 then ([(get_tile_loc tile_lst 8);(get_tile_loc tile_lst 9);
                  (get_tile_loc tile_lst 15)],[(get_tile_loc tile_lst 8);
                                               (get_tile_loc tile_lst 14);
                                               (get_tile_loc tile_lst 15)])
  else
  if v = 21 then ([(get_tile_loc tile_lst 21);(get_tile_loc tile_lst 22);
                   (get_tile_loc tile_lst 14)],[(get_tile_loc tile_lst 21);
                                                (get_tile_loc tile_lst 22);
                                                (get_tile_loc tile_lst 28)])
  else
  if v = 36 then ([(get_tile_loc tile_lst 36);(get_tile_loc tile_lst 29);
                   (get_tile_loc tile_lst 28)],[(get_tile_loc tile_lst 36);
                                                (get_tile_loc tile_lst 29);
                                                (get_tile_loc tile_lst 37)])
  else
  if v = 44 then ([(get_tile_loc tile_lst 44);(get_tile_loc tile_lst 38);
                   (get_tile_loc tile_lst 37)],[(get_tile_loc tile_lst 44);
                                                (get_tile_loc tile_lst 38);
                                                (get_tile_loc tile_lst 45)])
  else
  if v = 46 then ([(get_tile_loc tile_lst 46);(get_tile_loc tile_lst 39);
                   (get_tile_loc tile_lst 45)],[(get_tile_loc tile_lst 46);
                                                (get_tile_loc tile_lst 39);
                                                (get_tile_loc tile_lst 40)])
  else
  if v = 33 then ([(get_tile_loc tile_lst 33);(get_tile_loc tile_lst 32);
                   (get_tile_loc tile_lst 40)],[(get_tile_loc tile_lst 33);
                                                (get_tile_loc tile_lst 32);
                                                (get_tile_loc tile_lst 26)])
  else
  if v = 19 then ([(get_tile_loc tile_lst 19);(get_tile_loc tile_lst 18);
                   (get_tile_loc tile_lst 26)],[(get_tile_loc tile_lst 19);
                                                (get_tile_loc tile_lst 18);
                                                (get_tile_loc tile_lst 12)])
  else
  if v = 4 then ([(get_tile_loc tile_lst 4);(get_tile_loc tile_lst 11);
                  (get_tile_loc tile_lst 3)],[(get_tile_loc tile_lst 4);
                                              (get_tile_loc tile_lst 11);
                                              (get_tile_loc tile_lst 12)])
  else
    ([(get_tile_loc tile_lst 2);(get_tile_loc tile_lst 10);
      (get_tile_loc tile_lst 3)],[(get_tile_loc tile_lst 2);
                                  (get_tile_loc tile_lst 10);
                                  (get_tile_loc tile_lst 9)])

(** [gen_port_list tile_lst acc_lst tiles] returns a placement
    list based on the port locations. *)
let rec gen_port_list tile_lst acc_lst tiles=
  match tile_lst with
  | [] -> acc_lst
  | {resource = x; dienum = c; gridnum = v}::t -> if v = 8 ||
                                                     v = 36 || v = 21 ||
                                                     v = 44 || v = 2 ||
                                                     v = 4 || v = 46 ||
                                                     v = 19
    then
      let port_placement = Port (x, fst (gen_port_location_list tiles v)) in
      let second_port = Port (x, snd (gen_port_location_list tiles v)) in
      gen_port_list t (port_placement::second_port::acc_lst) tiles
    else gen_port_list t acc_lst tiles

(** [get_tile_resource tile_lst num] returns a resource base
    d on the gridnum location. *)
let rec get_tile_resource tile_lst num=
  match tile_lst with
  | [] -> Ocean
  | {resource = x; dienum = c; gridnum = v}::t -> if num = v then
      x else get_tile_resource t num

(** [last_port tile_lst port_list] returns the
    placement list for the last location. *)
let last_port tile_lst port_list =
  let port_location = Port (get_tile_resource tile_lst 33,
                            fst (gen_port_location_list tile_lst 33)) in
  let second_port = Port (get_tile_resource tile_lst 33,
                          snd (gen_port_location_list tile_lst 33)) in
  (port_location::second_port::port_list)

(** [edit_player st p] overwrites the player with the same name as [p] in
    [st.players] with [p]. *)
let edit_player st p =
  let rec replace_player p = function
    | [] -> failwith "Something has gone terribly wrong"
    | h::t -> if h.name = p.name then p::t else h::(replace_player p t) in
  let rec replace_owner p = function
    | [] -> []
    | h::t -> begin
        match h with
        | Building b -> (if b.player.name = p.name then
                           Building {player = p; location = b.location;
                                     build_type =
                                       b.build_type}::(replace_owner p t)
                         else
                           h::(replace_owner p t))
        | Port _ -> h::(replace_owner p t)
      end  in
  let rec change_player_in_order p = function
    | [] -> failwith "No players?"
    | (n, p1)::t -> if p.name = p1.name then (n,p)::t
      else (n,p1)::(change_player_in_order p t) in
  {players= replace_player p st.players; tiles=st.tiles;
   buildings= replace_owner p st.buildings;
   current_player= if p.name =st.current_player.name then p
     else st.current_player;
   player_order= change_player_in_order p st.player_order;
   dimensions=st.dimensions; turn=st.turn; start=st.start;
   roll_tiles=st.roll_tiles; expansion = st.expansion; deck = st.deck;
   robber = st.robber; longest_road = st.longest_road;
   has_drawn = st.has_drawn; has_used_card = st.has_used_card}

(** [edit_resource n r lst] is the association list of resources * int [lst]
    with the value corresponding to key [r] incremented by [n] (value as 0 if r
    not found). *)
let rec edit_resource n r lst = match lst with
  | [] -> [(r, n)]
  | (res, num) as h::t -> if res = r then (r, num+n)::t
    else h::(edit_resource n r t)

(** [increment_resource p n r] returns player [p] with [n] more of [r] in
    his/her inventory. *)
let increment_resource p n r = {p with
                                resources= edit_resource n r p.resources}

(** [is_valid_location loc] is true if the two or three tiles in [loc] are
    adjacent/have at least one common vertex and false otherwise. *)
let is_valid_location = function
  | a::b::c::[] -> begin
      if (a/7) mod 2 = 0 then
        if b - a = 1 || c - b = 1 then
          if c = a + 8 then true else false
        else
          false
      else
      if b - a = 1 || c - b = 1 then
        if c = a + 7 then true else false
      else
        false
    end
  | a::b::[] ->  begin
      if (a/7) mod 2 = 0 then
        if b- a = 1 || b - a = 7 || b - a = 8 then
          true
        else
          false
      else
      if b - a = 1 || b - a = 6 || b - a = 7 then
        true
      else
        false
    end
  | _ -> false

let on_land = function
  | a::b::c::[] -> (a.resource <> Ocean && a.dienum <> 21 && a.dienum <> 31) ||
                   (b.resource <> Ocean && b.dienum <> 21 && b.dienum <> 31) ||
                   (c.resource <> Ocean && c.dienum <> 21 && c.dienum <> 31)
  | a::b::[] -> (a.resource <> Ocean && a.dienum <> 21 && a.dienum <> 31) ||
                (b.resource <> Ocean && b.dienum <> 21 && b.dienum <> 31)
  | _ -> false

(** [spot_taken loc build_list] is false if no building in [build_list] has
    location [loc] and true otherwise. *)
let rec spot_taken loc build_list =
  match build_list with
  | [] -> false
  | h::t -> if h.location = loc then true else spot_taken loc t

(** [spot_taken_by_self p loc build_list] is true if player [p] owns a building
    in the specified location and false otherwise. *)
let rec spot_taken_by_self p loc build_list =
  match build_list with
  | [] -> false
  | h::t -> if h.location = loc then if h.player.name = p.name then true
      else false else spot_taken_by_self p loc t

(** [spot_taken_by_own_settlement p loc build_list] is true if player
    [p] owns a settlement in the specified location and false otherwise. *)
let rec spot_taken_by_own_settlement p loc build_list =
  match build_list with
  | [] -> false
  | h::t -> if h.location = loc then if h.player.name = p.name &&
                                        h.build_type = Settlement then true
      else false else spot_taken loc t


let has_road p l build_list =
  match l with
  | a::b::c::[] -> spot_taken_by_self p [a;b] build_list ||
                   spot_taken_by_self p [a;c] build_list ||
                   spot_taken_by_self p [b;c] build_list
  | _ -> false

let gen_adjacent_vertices_to_vertex = function
  | a::b::c::[] -> begin
      if (a/7) mod 2 = 0 then
        if b-a = 1 then
          if a/7 = 0 then
            ([b;c;a+9],[a;b+6;c],[a;b;c])
          else
            ([b;c;a+9],[a;b+6;c],[c-14;a;b])
        else
          ([c-9;a;b],[a;b-6;c],[b;c;a+14])
      else
      if b-a = 1 then
        if a mod 7 <> 1 then
          ([b;c;a+8],[a;b+5;c],[c-14;a;b])
        else
          ([b;c;a+8],[a;b;c],[c-14;a;b])
      else
      if c/ 7 <> 6 then
        ([b;c;a+14],[a;b-5;c],[c-8;a;b])
      else
        ([a;b;c],[a;b-6;c],[c-8;a;b])
    end
  | _ -> failwith "improper location called"

let rec tile_from_gridnum n tile_list =
  match tile_list with
  | [] -> failwith "trying to find tile that doesn't exist"
  | h::t -> if h.gridnum = n then h else tile_from_gridnum n t

let rec tile_list_from_gridnum_list tile_list = function
  | [] -> []
  | h::t -> (tile_from_gridnum h tile_list)::(tile_list_from_gridnum_list
                                                tile_list t)

let adjacent_vertices_free loc st =
  match loc with
  | a::b::c::[] -> let (x,y,z) = gen_adjacent_vertices_to_vertex loc in
    (spot_taken (tile_list_from_gridnum_list st.tiles x)
       (get_buildings st) = false)
    &&
    (spot_taken (tile_list_from_gridnum_list st.tiles y)
       (get_buildings st) = false)
    &&
    (spot_taken (tile_list_from_gridnum_list st.tiles z)
       (get_buildings st) = false)
  | _ -> false

let rec grab_gridnums = function
  | [] -> []
  | h::t -> h.gridnum::(grab_gridnums t)

let gen_adjacent_vertices_to_edge = function
  |a::b::[] -> begin
      if b - a = 1 then
        if (a/7) mod 2 = 0 then
          ([b-7;a;b], [a;b;b+7])
        else
          ([a-7;a;b], [a;b;a+7])
      else
      if (a/7) mod 2 = 0 then
        if a mod 7 = b mod 7 then
          ([a-1;a;b],[a;b;b+1])
        else
          ([a;a+1;b],[a;b-1;b])
      else
      if a mod 7 > b mod 7 then
        ([a-1;a;b],[a;b;b+1])
      else
        ([a;a+1;b],[a;b-1;b])
    end
  | _ -> failwith "improper location called"

let can_place_road p loc st =
  let (x,y) = gen_adjacent_vertices_to_edge loc in
  match tile_list_from_gridnum_list st.tiles loc with
  | a::b::[] -> (spot_taken_by_self p (tile_list_from_gridnum_list st.tiles x)
                   (get_buildings st) ||
                 spot_taken_by_self p (tile_list_from_gridnum_list st.tiles y)
                   (get_buildings st) ||
                 has_road p (tile_list_from_gridnum_list st.tiles x)
                   (get_buildings st) ||
                 has_road p (tile_list_from_gridnum_list st.tiles y)
                   (get_buildings st))
  | _ -> false


(** [check_location b l st] is true if the location of the building [b] is
    legal
    at location [l] in state st. *)
let check_location b l st =
  let sorted_loc = List.sort Pervasives.compare
      (grab_gridnums l) in
  if is_valid_location sorted_loc && on_land l then
    match b with
    | Settlement -> adjacent_vertices_free sorted_loc st &&
                    has_road st.current_player
                      (tile_list_from_gridnum_list st.tiles sorted_loc)
                      (get_buildings st) &&
                    spot_taken l (get_buildings st) = false
    | City ->
      spot_taken_by_own_settlement st.current_player
        (tile_list_from_gridnum_list st.tiles sorted_loc) (get_buildings st)
    | Road -> spot_taken l (get_buildings st) = false  &&
              can_place_road st.current_player sorted_loc st
    | Wall -> false
    | Ship -> false
  else
    false

(** [check_cost b st] is true if the current player has sufficient
    resources to
    build building [b] in st. *)
let check_cost b st = match b with
  | Settlement -> begin
      (List.assoc Wood st.current_player.resources) >= 1 &&
      (List.assoc Sheep st.current_player.resources) >= 1 &&
      (List.assoc Brick st.current_player.resources) >= 1 &&
      (List.assoc Grain st.current_player.resources) >= 1
    end
  | City -> begin
      (List.assoc Rock st.current_player.resources) >=3 &&
      (List.assoc Grain st.current_player.resources) >= 2
    end
  | Road -> (List.assoc Wood st.current_player.resources) >= 1 &&
            (List.assoc Brick st.current_player.resources) >= 1
  | Ship -> false
  | Wall -> false

(** [building_on_tile b tile] is true if the building [b] is on a vertex of
    [tile] and false otherwise *)
let building_on_tile b tile = List.mem tile b.location

(** [harvest st tile] attributes resources to every player with a building
    adjacent to [tile] in st according to the rules (1 for settlements, 2 for
    cities). *)
let harvest st tile =
  let rec resource_collect bld_list tile acc = match bld_list with
    | [] -> acc
    | h::t ->  if building_on_tile h tile then begin
        match h.build_type with
        | Settlement -> resource_collect t tile
                          (edit_player acc (increment_resource h.player 1
                                              tile.resource))
        | City -> resource_collect t tile
                    (edit_player acc (increment_resource h.player
                                        2 tile.resource))
        | _ -> resource_collect t tile acc
      end
      else resource_collect t tile acc
  in resource_collect (get_buildings st) tile st

let has_won st =
  let rec check_scores n = function
    | [] -> None
    | h::t -> if h.score >= n then Some h else check_scores n t in
  match st.expansion with
  | Basic -> match check_scores 10 st.players with
    | None -> Legal st
    | Some p -> Winner p


(** [has_too_many_resources player] returns the number of resources the player
    must discard*)
let has_too_many_resources p n = let res_list = p.resources in
  let num_resources = (List.assoc Brick res_list) + (List.assoc Wood res_list)
                      + (List.assoc Sheep res_list) +
                      (List.assoc Grain res_list) +
                      (List.assoc  Rock res_list) in
  if num_resources > n then num_resources/2 else 0

let get_player_from_name name st =
  let rec check_name n = function
    | [] -> failwith "no players of that name"
    | h::t ->
      if h.name = n then h else check_name n t
  in
  check_name name st.players

let rec track_worth acc lst =
  match lst with
  | [] -> acc
  | (r, n)::t -> if r <> Ocean then track_worth (acc + n) t
    else track_worth (acc) t

let discard player_name choices st =
  let rec can_pay_claim st = function
    | [] -> true
    | (r,n)::t -> if List.assoc r
        (get_player_from_name player_name st).resources >= n then
        can_pay_claim st t else false in
  let rec process_choices choices acc = match choices with
    | [] -> acc
    | (resource, amount)::t ->
      if can_pay_claim acc choices then (
        let discarded_state = edit_player acc
            (increment_resource
               (get_player_from_name player_name acc)
               (-amount) resource) in
        process_choices t discarded_state
      )
      else
        raise (Failure "player doesn't have enough to discard")
  in
  try Legal (process_choices choices st) with Failure _ -> Illegal

let move_robber loc st =
  let target_tile = tile_from_gridnum loc st.tiles in
  if target_tile.dienum = 0 || target_tile.dienum = 21 ||
     target_tile.dienum = 31 || target_tile.gridnum = st.robber then
    Illegal
  else
    Legal {st with robber = loc}

let rec player_on_tiles b_list tile acc_lst=
  match b_list with
  | [] -> acc_lst
  | h::t -> begin
      match h with
      | Building b -> if building_on_tile b tile && b.build_type <> Road then
          player_on_tiles t tile (b.player.name::acc_lst)
        else player_on_tiles t tile acc_lst
      | Port _ -> player_on_tiles t tile acc_lst
    end

(** [has_resources p] is true if p has resources and false otherwise. *)
let has_resources p =
  let rec more_than_one = function
    | [] -> false
    | (_,n)::t -> if n > 0 then true else more_than_one t in
  more_than_one p.resources

(** [adj_with_resources st players] is a list of players who are adjacent 
    and have resources. *)
let adj_with_resources st players =
  let rec remove_broke st acc = function
    | [] -> acc
    | h::t -> if has_resources (get_player_from_name h st) then
        remove_broke st (h::acc) t else remove_broke st acc t in
  remove_broke st [] players

(** [get_players_to_steal st] gets the list of players to be stolen from. *)
let get_players_to_steal st =
  (List.sort_uniq Pervasives.compare (player_on_tiles (st.buildings)
                                        (get_tile_loc st.tiles st.robber) []))
  |> adj_with_resources st

(** [gen_stealable_list acc (res,n)] is the list of resources which are
    stealable.  *)
let rec gen_stealable_list acc (res, n) = if n > 0 then gen_stealable_list
      ((res,n)::acc) (res,n-1) else acc

let stealable_list reslist =
  let rec make_list acc = function
    | [] -> acc
    | (res,n)::t -> make_list ((gen_stealable_list [] (res,n))@acc) t in
  make_list [] reslist

let rec steal player_name st =
  let target = get_player_from_name player_name st in
  let prob_list = stealable_list target.resources in
  let rand = QCheck.Gen.int_range 0 ((List.length prob_list) -1) in
  let (res, _) = List.nth prob_list (QCheck.Gen.generate1 rand) in
  (Legal (edit_player st (increment_resource
                            (get_player_from_name player_name st)
                            (-1) res))), res

let charge_price p = function
  | Settlement -> increment_resource
                    (increment_resource
                       (increment_resource
                          (increment_resource p (-1) Wood)
                          (-1) Sheep) (-1) Grain) (-1) Brick
  | City -> increment_resource (increment_resource p (-2) Grain) (-3) Rock
  | Road -> increment_resource (increment_resource p (-1) Wood) (-1) Brick
  | Ship -> p
  | Wall -> p

let increment_score p = function
  | Settlement -> {p with
                   score = p.score + 1}
  | City -> { p with
              score = p.score + 1;}
  | Road -> p
  | Ship -> p
  | Wall -> p

let rec remove_settlement loc  = function
  | [] -> []
  | h::t -> begin
      match h with
      | Building b -> if b.location = loc && b.build_type =
                                             Settlement then t
        else h::(remove_settlement loc t)
      | Port _ -> h::(remove_settlement loc t)
    end

let remove_settlement_from_state loc st =
  {players=st.players; tiles=st.tiles;
   buildings=remove_settlement loc st.buildings;
   current_player=st.current_player; player_order=st.player_order;
   turn=st.turn; dimensions=st.dimensions; start=st.start;
   roll_tiles=st.roll_tiles; expansion = st.expansion; deck = st.deck;
   robber=st.robber; longest_road=st.longest_road; has_drawn=st.has_drawn;
   has_used_card = st.has_used_card}

let insert_building b st =
  {players=st.players; tiles=st.tiles; buildings=Building b::st.buildings;
   current_player=st.current_player; player_order=st.player_order;
   turn=st.turn; dimensions=st.dimensions; start=st.start;
   roll_tiles=st.roll_tiles; expansion = st.expansion;
   deck = st.deck; robber = st.robber; longest_road = st.longest_road;
   has_drawn=st.has_drawn; has_used_card = st.has_used_card }

let rec count_knights hand acc =
  match hand with
  | [] -> acc
  | h::t -> (match h with
      | Knight reco -> if reco.played then count_knights t (acc+1)
        else count_knights t acc
      | _ -> count_knights t acc)

let rec get_player_with_largest_army players p_w_la =
  match players with
  | [] -> p_w_la
  | h::t -> if ((count_knights h.cards 0) > (count_knights p_w_la.cards 0))
    then get_player_with_largest_army t h else get_player_with_largest_army
        t p_w_la

let get_largest_army st =
  match st.players with
  | [] -> failwith "Something went wrong"
  | h::t -> get_player_with_largest_army st.players h

let accredit_largest_army st st' =
  if get_largest_army st = get_largest_army st' then st'
  else
    let loser = get_largest_army st in
    let winner = get_largest_army st' in
    if count_knights loser.cards 0 > 3 then
      edit_player (edit_player st' {loser with
                                    score=loser.score -2})
        {winner with score=winner.score + 2}
    else
    if count_knights winner.cards 0 > 3 then
      edit_player st' {winner with score = winner.score + 2}
    else
      st'

let rec get_roads acc = function
  | [] -> acc
  | h::t -> if h.build_type = Road then get_roads (h::acc) t
    else get_roads acc t

let get_adjacent_edges l =
  match l with
  | a::b::c::[] -> ([a;b], [a;c], [b;c])
  | _ -> failwith "misused by developers"

let rec building_at_location l = function
  | [] -> failwith "thought there was something there but nope"
  | h::t -> if h.location = l then h else building_at_location l t

let rec traverse road visited tiles bldngs length =
  let (x,y) = gen_adjacent_vertices_to_edge (grab_gridnums road.location) in
  let xtiles = tile_list_from_gridnum_list tiles x in
  let ytiles = tile_list_from_gridnum_list tiles y in
  let xadj = get_adjacent_edges xtiles in
  let yadj = get_adjacent_edges ytiles in
  let max_branch tile_set adj_edges =
    if (spot_taken_by_self road.player tile_set bldngs
        || spot_taken tile_set bldngs = false) then
      match adj_edges with
      | (a,b,c) -> begin
          if spot_taken_by_self road.player a bldngs then
            let blda = building_at_location a bldngs in
            if List.mem blda visited = false then
              if spot_taken_by_self road.player b bldngs then
                let bldb = building_at_location b bldngs in
                if List.mem bldb visited = false then
                  max (fst (traverse blda (blda::visited) tiles bldngs
                              (length + 1)))
                    (fst (traverse bldb (bldb::visited) tiles bldngs
                            (length + 1)))
                else
                if spot_taken_by_self road.player c bldngs && List.mem
                     (building_at_location c bldngs) visited = false  then
                  let bldc = building_at_location c bldngs in
                  max (fst (traverse blda (blda::visited) tiles bldngs
                              (length + 1)))
                    (fst (traverse bldc (bldc::visited) tiles bldngs
                            (length + 1)))
                else
                  fst (traverse blda (blda::visited) tiles bldngs (length+1))
              else
              if spot_taken_by_self road.player c bldngs && List.mem
                   (building_at_location c bldngs) visited = false then
                fst (traverse blda (blda::visited) tiles bldngs (length+1))
              else 0
            else
            if spot_taken_by_self road.player b bldngs && List.mem
                 (building_at_location b bldngs) bldngs = false then
              let bldb = building_at_location b bldngs in
              if spot_taken_by_self road.player c bldngs then
                let bldc = building_at_location c bldngs in
                max (fst (traverse bldb (bldb::visited) tiles bldngs
                            (length + 1)))
                  (fst (traverse bldc (bldc::visited) tiles bldngs
                          (length + 1)))
              else
                fst (traverse bldb (bldb::visited) tiles bldngs (length+1))
            else
            if spot_taken_by_self road.player c bldngs && List.mem
                 (building_at_location c bldngs) visited = false then
              let bldc = building_at_location c bldngs in
              fst (traverse bldc (bldc::visited) tiles bldngs (length+1))
            else 0
          else
          if spot_taken_by_self road.player b bldngs then
            let bldb = building_at_location b bldngs in
            if List.mem bldb visited = false then
              fst (traverse bldb (bldb::visited) tiles bldngs (length+1))
            else
            if spot_taken_by_self road.player c bldngs && List.mem
                 (building_at_location c bldngs) bldngs = false then
              let bldc = building_at_location c bldngs in
              fst (traverse bldc (bldc::visited) tiles bldngs (length+1))
            else 0
          else 0
        end
    else 0
  in
  let maxx = max_branch xtiles xadj in
  let maxy = max_branch ytiles yadj in
  ((max maxx maxy) + 1, road.player.name)

let rec check_all_roads roadlist current_longest p tiles bldngs =
  match roadlist with
  | [] -> (current_longest, p)
  | h::t -> let length = fst (traverse h [h] tiles bldngs 0) in
    if length > current_longest && length >= 5 then
      check_all_roads t (fst (traverse h [h] tiles bldngs 0))
        (Some h.player.name) tiles bldngs
    else
      check_all_roads t current_longest p tiles bldngs

(** [find_longest_road st n] returns [Some player_name] if
    [player_name] has the
    longest road with length greater than [n] or [None] otherwise.
*)
let find_longest_road st =
  let bldngs = get_buildings st in
  let tiles = get_tiles st in
  let roadlist = get_roads [] bldngs in
  let (l,p) = check_all_roads roadlist 0 None tiles bldngs in
  (l,p)

(** [accredit_longest_road st st'] returns a state with score changes made as
    necessary based on a change in who has longest road between [st] and [st']
*)
let accredit_longest_road st st' =
  if snd (find_longest_road st) = snd (find_longest_road st') then st'
  else
    match snd (find_longest_road st') with
    | None -> (match snd (find_longest_road st) with
        | None -> st'
        | Some loser ->  let player_loser = get_player_from_name loser st in
          {(edit_player st' {player_loser with
                             score=player_loser.score -2}) with
           longest_road = 0})
    | Some p -> let st'' = let player_winner = get_player_from_name p st' in
                  {(edit_player st' {player_winner with
                                     score = player_winner.score + 2}) with
                   longest_road = fst (find_longest_road st')} in
      match snd (find_longest_road st) with
      | None -> st''
      | Some loser -> let player_loser = get_player_from_name loser st in
        edit_player st'' {player_loser with
                          score=player_loser.score -2}

let terminate_here = function
  | a::b::[] -> if (a mod 7 = 0 && b mod 7 = 0) || (a/7 = 0 && abs (a-b) = 1)||
                   (a mod 7 = 6 && b mod 7 = 6) || (a/7 = 6 && abs (a-b) = 1)
    then true else false
  | _ -> true

(** [shortest_path edge goal path tiles bldngs length player] returns
    the shortest path from [edge] to [goal] and the length of [path]. *)
let rec shortest_path edge goal path tiles bldngs length player n =
  if terminate_here edge || length > n then 999, []
  else
    let (x,y) = gen_adjacent_vertices_to_edge edge in
    let sorted_goal = List.sort Pervasives.compare goal in
    if x = sorted_goal || y = sorted_goal then (length, path) else
      let xtiles = tile_list_from_gridnum_list tiles x in
      let ytiles = tile_list_from_gridnum_list tiles y in
      let xadj = get_adjacent_edges x in
      let yadj = get_adjacent_edges y in
      let min_branch tile_set adj_edges =
        if (spot_taken_by_self player tile_set bldngs
            || spot_taken tile_set bldngs = false) then
          match adj_edges with
          | (a,b,c) -> begin
              if List.mem a path = false then
                let (lengtha, patha) = if spot_taken_by_self player
                    (tile_list_from_gridnum_list tiles a) bldngs then
                    shortest_path a goal (a::path) tiles bldngs
                      length player n
                  else if spot_taken (tile_list_from_gridnum_list tiles a)
                      bldngs = false then
                    shortest_path a goal (a::path) tiles bldngs (length+1)
                      player n
                  else
                    999, []
                in
                if List.mem b path = false then
                  let (lengthb, pathb) = if spot_taken_by_self player
                      (tile_list_from_gridnum_list tiles b) bldngs then
                      shortest_path b goal (b::path) tiles bldngs
                        length player n
                    else if spot_taken (tile_list_from_gridnum_list tiles b)
                        bldngs = false then
                      shortest_path b goal (b::path) tiles bldngs (length+1)
                        player n
                    else
                      999, []
                  in
                  if List.mem c path = false then
                    let (lengthc, pathc) = if spot_taken_by_self player
                        (tile_list_from_gridnum_list tiles c) bldngs then
                        shortest_path c goal (c::path) tiles bldngs length
                          player n
                      else if spot_taken (tile_list_from_gridnum_list tiles c)
                          bldngs = false then
                        shortest_path c goal (c::path) tiles bldngs (length+1)
                          player n
                      else
                        999, []
                    in
                    let chosen = min lengthc (min lengtha lengthb) in
                    if chosen = lengtha then
                      lengtha, patha
                    else if chosen = lengthb then
                      lengthb, pathb
                    else
                      lengthc, pathc
                  else
                    let chosen = min lengtha lengthb in
                    if chosen = lengtha then
                      lengtha, patha
                    else
                      lengthb, pathb
                else
                if List.mem c path = false then
                  let (lengthc, pathc) = if spot_taken_by_self player
                      (tile_list_from_gridnum_list tiles c) bldngs then
                      shortest_path c goal (c::path) tiles bldngs
                        length player n
                    else if spot_taken (tile_list_from_gridnum_list tiles c)
                        bldngs = false then
                      shortest_path c goal (c::path) tiles bldngs (length+1)
                        player n
                    else
                      999, []
                  in
                  let chosen = min lengtha lengthc in
                  if chosen = lengtha then
                    lengtha, patha
                  else
                    lengthc, pathc
                else
                  lengtha, patha
              else
              if List.mem b path = false then
                let (lengthb, pathb) = if spot_taken_by_self player
                    (tile_list_from_gridnum_list tiles b) bldngs then
                    shortest_path b goal (b::path) tiles bldngs
                      length player n
                  else if spot_taken (tile_list_from_gridnum_list tiles b)
                      bldngs = false then
                    shortest_path b goal (b::path) tiles bldngs (length+1)
                      player n
                  else
                    999, []
                in
                if List.mem c path = false then
                  let (lengthc, pathc) = if spot_taken_by_self player
                      (tile_list_from_gridnum_list tiles c) bldngs then
                      shortest_path c goal (c::path) tiles bldngs
                        length player n
                    else if spot_taken (tile_list_from_gridnum_list tiles c)
                        bldngs = false then
                      shortest_path c goal (c::path) tiles bldngs (length+1)
                        player n
                    else
                      999, []
                  in
                  let chosen = min lengthb lengthc in
                  if chosen = lengthb then
                    lengthb, pathb
                  else
                    lengthc, pathc
                else
                if List.mem c path = false then
                  let (lengthc, pathc) = if spot_taken_by_self player
                      (tile_list_from_gridnum_list tiles c) bldngs then
                      shortest_path c goal (c::path) tiles bldngs length
                        player n
                    else if spot_taken (tile_list_from_gridnum_list tiles c)
                        bldngs = false then
                      shortest_path c goal (c::path) tiles bldngs (length+1)
                        player n
                    else
                      999, []
                  in
                  lengthc, pathc
                else
                  999, []
              else
                999, []
            end
        else 999, []
      in
      let (minx, pathx) = min_branch xtiles xadj in
      let (miny, pathy) = min_branch ytiles yadj in
      let chosen = min minx miny in
      if chosen = minx then
        minx, pathx
      else
        miny, pathy

let path_from_settlement_to_vertex start player_name goal st range =
  let (x,y,z) = get_adjacent_edges start in
  let (xlength, xpath) = shortest_path x goal [x] st.tiles (get_buildings st) 1
      (get_player_from_name player_name st) range in
  let (ylength, ypath) = shortest_path y goal [y] st.tiles (get_buildings st) 1
      (get_player_from_name player_name st) range in
  let (zlength, zpath) = shortest_path z goal [z] st.tiles (get_buildings st) 1
      (get_player_from_name player_name st) range in
  let chosen = min zlength (min xlength ylength) in
  if chosen = xlength then
    xlength, (List.rev xpath)
  else if chosen = ylength then
    ylength, (List.rev ypath)
  else
    zlength, (List.rev zpath)

(* For a given edge, if it is visited then ignore it. If it is a road
   owned by another player then set length to max and path to [].
    If it's owned by this player, add the edge to the path but
    keep length the same, and traverse that child. If it's an empty edge,
    add it to the path, add 1 to length, and traverse it. *)

(* This section handles bonus cards *)

(** [has_been_played card] is true if card has been played and false otherwise.
*)
let has_been_played card =
  match card with
  | VP rec1 -> rec1.played
  | Knight rec1 -> rec1.played
  | Progress rec1 -> rec1.played

let rec filter_cards card_lst acc_lst =
  match card_lst with
  | [] -> acc_lst
  | h::t -> if has_been_played h
    then filter_cards t acc_lst
    else filter_cards t (h::acc_lst)

let get_current_player_cards st =
  filter_cards st.current_player.cards []

(** [initial_deck_of_bonus_cards] is the initial card list representing the
    initial hardcoded deck of bonus cards. *)
let initial_deck_of_bonus_cards =
  let cards =
    [
      VP {played = false; pair = ("University",1)};
      VP {played = false; pair = ("Palace",1)};
      VP {played = false; pair = ("Market",1)};
      VP {played = false; pair = ("Chapel",1)};
      VP {played = false; pair = ("Library",1)};
      Knight {played = false};Knight {played = false};Knight {played = false};
      Knight {played = false};Knight {played = false};Knight {played = false};
      Knight {played = false};Knight {played = false};Knight {played = false};
      Knight {played = false};Knight {played = false};Knight {played = false};
      Knight {played = false};Knight {played = false};
      Progress {played = false; progress = YearOfPlenty};
      Progress {played = false; progress = YearOfPlenty};
      Progress {played = false; progress = Monopoly};
      Progress {played = false; progress = Monopoly};
      Progress {played = false; progress = RoadBuilding};
      Progress {played = false; progress = RoadBuilding}
    ] in
  let rand_order = QCheck.Gen.shuffle_l cards in
  let usable_order = QCheck.Gen.generate1 rand_order in
  usable_order

let string_to_card str =
  match str with
  | "university" -> VP {played = false; pair = ("University",1)}
  | "palace" -> VP {played = false; pair = ("Palace",1)}
  | "market" -> VP {played = false; pair = ("Market",1)}
  | "chapel" -> VP {played = false; pair = ("Chapel",1)}
  | "library" -> VP {played = false; pair = ("Library",1)}
  | _ -> failwith "Only use this function for VP card strings"

let card_to_string (card : card) : string =
  match card with
  | VP reco -> (string_of_int (snd reco.pair))^
               " Victory Point - "^(fst reco.pair)
  | Knight _ -> "Knight"
  | Progress reco -> match reco.progress with
    | YearOfPlenty -> "Year of Plenty"
    | Monopoly -> "Monopoly"
    | RoadBuilding -> "Road Builder"

let check_card_cost st =
  begin
    (List.assoc Sheep st.current_player.resources) >= 1 &&
    (List.assoc Grain st.current_player.resources) >= 1 &&
    (List.assoc Rock st.current_player.resources) >= 1
  end

(** [set_card_played card] is card but with the played field set to true. *)
let set_card_played (card : card) =
  match card with
  | VP reco -> VP {reco with played = true}
  | Knight _ -> Knight {played = true}
  | Progress reco -> Progress {reco with played = true}

(** [are_cards_equal card1 card2] is true if card [card1] and card [card2] are
    equal and false otherwise. *)
let are_cards_equal card1 card2 =
  match card1, card2 with
  | (VP rec1, VP rec2) ->
    fst rec1.pair = fst rec2.pair && rec1.played = rec2.played
  | (Knight rec1, Knight rec2) -> rec1.played  = rec2.played
  | (Progress rec1, Progress rec2) ->
    rec1.progress = rec2.progress && rec1.played = rec2.played
  | _ -> false

(** [update_player_hand deck card adding] is deck with card added if adding is
    true and deck with card [card] set to played if adding is false. *)
let rec update_player_hand deck card adding =
  match deck with
  | [] -> if adding then (card::[]) else []
  | h::t -> if adding then card::h::t else if are_cards_equal card h
    then ((set_card_played card)::t)
    else (h::update_player_hand t card adding)

(** [pick_card st] is a tuple containing the state that results from drawing
    a card from the deck and the card that was randomly drawn. *)
let rec pick_card st =
  if check_card_cost st && st.has_drawn = false then (
    match st.deck with
    | [] -> pick_card {st with deck = initial_deck_of_bonus_cards}
    | h::t -> let st' = edit_player st
                  {st.current_player with cards =
                                            (update_player_hand
                                               st.current_player.cards
                                               h true)} in
      (Legal {st' with deck = t}, (card_to_string h)))
  else Illegal, ""

let draw_card st =
  match pick_card st with
  | Legal st',str ->
    Legal (edit_player st' (increment_resource
                              (increment_resource
                                 (increment_resource
                                    st'.current_player (-1) Rock)
                                 (-1) Sheep) (-1) Grain)),str
  | Illegal,_ -> Illegal,""
  | _ -> failwith "Something went very wrong"

(** [mutate_score p num] is player [p] with [p]'s score incremented by num. Num
    can be positive or negative. *)
let mutate_score p num =
  {p with score = p.score + num}

(** [check_hand card_lst card] is true if card_lst contains card and false
    otherwise. *)
let rec check_hand card_lst card =
  match card_lst with
  | [] -> false
  | h::t -> match (h,card) with
    | (VP rec1, VP rec2) -> (((fst rec1.pair) =
                              (fst rec2.pair)) &&
                             not(has_been_played card)) || check_hand t card
    | (Knight _, Knight _) -> (true && not(has_been_played card))
                              || check_hand t card
    | (Progress rec1, Progress rec2) ->
      ((rec1.progress = rec2.progress) && not(has_been_played card))
      || check_hand t card
    | _ -> false || check_hand t card

(** [has_card p card] is true if player [p] has card [card] and false
    otherwise. *)
let has_card p card =
  check_hand p.cards card

(** [play_vp st str] is Legal of the state that results from current player
    playing the victory point card associated with str if the player has the
    specified card. It is Illegal otherwise. *)
let play_vp st str =
  if st.has_used_card then
    Illegal
  else
    let card = string_to_card str in
    if has_card st.current_player card then (
      let player' = mutate_score st.current_player 1 in
      let st' = (edit_player st
                   {player' with cards =
                                   update_player_hand
                                     player'.cards card false })
      in
      let st' = {st' with has_used_card = true} in
      has_won st'
    )
    else Illegal

let play_yop st (res1,res2) =
  if st.has_used_card then
    Illegal
  else
    let card = Progress {played = false; progress = YearOfPlenty} in
    if has_card st.current_player card then (
      let st' = (edit_player st
                   (increment_resource
                      (increment_resource st.current_player (1) res1)
                      (1) res2))
      in
      let st' = {st' with has_used_card = true} in
      Legal
        (edit_player st'
           {st'.current_player with cards =
                                      update_player_hand
                                        st'.current_player.cards
                                        card false})
    ) else Illegal

let play_monopoly st res =
  if st.has_used_card then
    Illegal
  else
    let card = Progress {played = false; progress = Monopoly} in
    if has_card st.current_player card then (
      let p = st.current_player.name in
      let rec exchanges_complete st p res players = match players with
        | [] -> st
        | h::t -> if h.name = p then exchanges_complete st p res t else
            let n = List.assoc res h.resources in
            exchanges_complete
              (edit_player (edit_player st
                              (increment_resource h (-n) res))
                 (increment_resource
                    (get_player_from_name p st) n res)) p res t in
      let st' = exchanges_complete st p res st.players in
      let st' = {st' with has_used_card = true} in
      let st' = (edit_player st'
                   {st'.current_player with cards =
                                              update_player_hand
                                                st'.current_player.cards
                                                card false}) in
      Legal st'
    ) else Illegal

let bonus_road loc st card =
  let sorted_loc =
    List.sort Pervasives.compare loc in
  if check_location Road (tile_list_from_gridnum_list st.tiles sorted_loc) st
  then
    let st' = (accredit_longest_road st (insert_building
                                           {player= st.current_player;
                                            location=
                                              tile_list_from_gridnum_list
                                                st.tiles sorted_loc;
                                            build_type=Road}
                                           st)) in
    let st' = {st' with has_used_card = true} in
    let st' = (edit_player st'
                 {st'.current_player with cards =
                                            update_player_hand
                                              st'.current_player.cards
                                              card false}) in
    has_won st'
  else Illegal

let play_rb st (loc1, loc2) =
  if st.has_used_card then
    Illegal
  else
    let card = Progress {played = false; progress = RoadBuilding} in
    if has_card st.current_player card then
      match bonus_road loc1 st card with
      | Legal st' -> bonus_road loc2 st' card
      | _ -> Illegal
    else
      Illegal

let play_knight st =
  if st.has_used_card then
    Illegal
  else
    let card = Knight {played = false} in
    if has_card st.current_player card then
      let st' = {st with has_used_card = true} in
      let st'' = accredit_longest_road st
          (edit_player st'
             {st'.current_player with cards =
                                        update_player_hand
                                          st'.current_player.cards card false})
      in
      Prompt (Discard ([], st''))
    else
      Illegal

(* Bonus card section end *)

let init
    (expansion : expansion)
    (players : player list)
    (player_order: (int * player) list) =
  Random.self_init ();
  let tiles = gen_board expansion in
  {players = players;
   tiles = sort_by_gridnum tiles;
   (* tiles = gen_board Basic; *)
   buildings = last_port tiles (gen_port_list tiles [] tiles);
   current_player = List.assoc 0 player_order;
   player_order= player_order; turn=0;
   dimensions = (7, 7); start=false; roll_tiles = get_roll_tiles tiles;
   expansion = expansion; deck = initial_deck_of_bonus_cards;
   robber = find_desert_tile tiles; longest_road = 0; has_drawn = false;
   has_used_card = false }



let start st = if st.start then Illegal, 0
  else
    let d = dice_roll () in
    let tiles = List.assoc d st.roll_tiles in
    if d = 7 then
      let rec calculate_losses acc = function
        | [] -> acc
        | h::t-> let disc = has_too_many_resources h 7 in if disc <> 0 then
            calculate_losses ((h,disc)::acc) t else calculate_losses acc t in
      (Prompt (Discard ((calculate_losses [] st.players),
                        {st with start = true})), 7)
    else
      let rec harvest_from_tiles acc = function
        | [] -> acc
        | h::t -> if acc.robber <> h.gridnum then harvest_from_tiles
              (harvest acc h) t else harvest_from_tiles acc t
      in
      let without_start_change = harvest_from_tiles st tiles in
      Legal {without_start_change with start = true}, d


let next_player st =
  List.assoc ((st.turn + 1) mod (List.length st.players)) st.player_order

(** [end_turn t] is the result containing the state after a player's turn has
    ended if the player's turn had not yet started and illegal otherwise. *)
let end_turn st = if st.start then
    Legal {players=st.players; tiles=st.tiles; buildings=st.buildings;
           current_player=next_player st; player_order=st.player_order;
           turn=st.turn + 1; dimensions=st.dimensions; start=false;
           roll_tiles=st.roll_tiles; expansion = st.expansion; deck = st.deck;
           robber=st.robber; longest_road=st.longest_road;
           has_drawn= false; has_used_card = false}
  else Illegal

let init_placements b loc st player_name=
  let sorted_loc =
    List.sort Pervasives.compare (grab_gridnums loc) in
  let player = get_player_from_name player_name st in
  let st' = edit_player st (increment_score player b) in
  match b with
  | Settlement -> if spot_taken loc (get_buildings st) = false &&
                     is_valid_location sorted_loc && on_land loc &&
                     adjacent_vertices_free sorted_loc st
    then has_won
        (insert_building
           {player=player;
            location=tile_list_from_gridnum_list
                st.tiles sorted_loc;build_type=b}
           st')
    else Illegal
  | _ -> Illegal




let initial_roads loc settle st p =
  let sorted_loc =
    List.sort Pervasives.compare loc in
  let sorted_settle = List.sort Pervasives.compare settle in
  let player = get_player_from_name p st in
  let (x,y) = gen_adjacent_vertices_to_edge sorted_loc in
  if spot_taken (tile_list_from_gridnum_list st.tiles sorted_loc)
      (get_buildings st) = false  &&
     can_place_road player sorted_loc st
     && (x = sorted_settle || y = sorted_settle) then
    Legal (insert_building
             {player=player;location=tile_list_from_gridnum_list
                                st.tiles sorted_loc;build_type=Road}
             st)
  else Illegal

(** [build b l t] is the result containing the new state after a player has
    built a new building of type [b] at location [l] or illegal if that action
    was not possible*)
let build b loc st =
  let sorted_loc = tile_list_from_gridnum_list st.tiles
      (List.sort Pervasives.compare (grab_gridnums loc)) in
  let st' =
    edit_player st (increment_score (charge_price st.current_player b) b) in
  if check_cost b st && check_location b sorted_loc st
  then has_won (accredit_longest_road st'
                  (insert_building
                     {player=st'.current_player;
                      location=sorted_loc;build_type=b}
                     (remove_settlement_from_state loc st')))
  else Illegal

(** [scores t] is an association list
    mapping players' names to their scores. *)
let scores st =
  let rec ext_scores = function
    | [] -> []
    | h::t -> (h.name,h.score) :: (ext_scores t) in
  ext_scores st.players

(** [resource_list res_list] converts an association list of type
     (resource * int) list to one of (string * int) list. *)
let resource_list res_list =
  [("Brick", List.assoc Brick res_list);("Wood", List.assoc Wood res_list);
   ("Sheep", List.assoc Sheep res_list);("Grain", List.assoc Grain res_list);
   ("Rock", List.assoc  Rock res_list)]

(** [resources t] returns the association list mapping resource names to the
    amount the current player has. *)
let resources st = resource_list st.current_player.resources


let rec num_of_each st acc = function
  | [] -> acc
  | h::t ->
    if List.mem_assoc h.resource acc then
      if h.dienum = 21 || h.dienum = 31 || h.dienum = 7 || h.dienum = 0 then
        num_of_each st acc t
      else
        num_of_each st
          ((h.resource, 1 + (List.assoc h.resource acc))::
           (List.remove_assoc h.resource acc)) t
    else
    if h.dienum = 21 || h.dienum = 31 || h.dienum = 7 || h.dienum = 0 then
      num_of_each st acc t
    else
      num_of_each st ((h.resource, 1)::acc) t

let resources_from_placement player_name loc st =
  let p = get_player_from_name player_name st in
  let res_list = num_of_each st [] loc in
  let rec assignment st res_list p =
    match res_list with
    | [] -> st
    | (r, n)::t -> assignment (edit_player st (increment_resource p n r)) t
                     (increment_resource p n r)
  in assignment st res_list p

(* TRADING;
   on_ports
   get_rates
   trade
   accept
*)

let rec on_ports acc p tiles bldngs = function
  | [] -> acc
  | Port (r,l)::t -> if spot_taken_by_self p (sort_by_gridnum l) bldngs
    then
      on_ports (r::acc) p tiles bldngs t else on_ports acc p tiles bldngs t
  | Building _ :: t -> failwith
                         "we should have already removed all buildings"

let get_rates port_list = if List.mem Ocean port_list then
    [(Brick, if List.mem Brick port_list then 2 else 3);
     (Wood, if List.mem Wood port_list then 2 else 3);
     (Grain, if List.mem Grain port_list then 2 else 3);
     (Sheep, if List.mem Sheep port_list then 2 else 3);
     (Rock, if List.mem Rock port_list then 2 else 3)] else
    [(Brick, if List.mem Brick port_list then 2 else 4);
     (Wood, if List.mem Wood port_list then 2 else 4);
     (Grain, if List.mem Grain port_list then 2 else 4);
     (Sheep, if List.mem Sheep port_list then 2 else 4);
     (Rock, if List.mem Rock port_list then 2 else 4)]

let trade give receive target st player_name =
  if (String.lowercase_ascii target) = "bank" then
    let rec quant_desired acc = function
      | [] -> acc
      | (r,n)::t -> quant_desired (n+acc) t in
    let quant = quant_desired 0 receive in
    let ports = on_ports [] (get_player_from_name player_name st) st.tiles
        (get_buildings st) (get_ports st)
    in
    let rates = get_rates ports in
    let rec track_worth acc = function
      | [] -> acc
      | (r, n)::t -> track_worth (acc + (n/(List.assoc r rates))) t in
    let rec can_pay_claim st = function
      | [] -> true
      | (r,n)::t -> if List.assoc r
          (get_player_from_name player_name st).resources >= n then
          can_pay_claim st t else false in
    if track_worth 0 give >= quant && can_pay_claim st give then
      let rec transaction pay_or_gain st = function
        | [] -> st
        | (r,n)::t -> transaction pay_or_gain
                        (edit_player st
                           (increment_resource
                              (get_player_from_name player_name st)
                              (if pay_or_gain then (-n) else n) r)) t in
      Legal (transaction false (transaction true st give) receive)
    else
      Illegal
  else
    let rec can_pay_claim st = function
      | [] -> true
      | (r,n)::t -> if List.assoc r
          (get_player_from_name player_name st).resources >= n then
          can_pay_claim st t else false in
    if can_pay_claim st give then
      let proposal =
        Trade ((get_player_from_name target st), give,receive,st) in
      Prompt proposal
    else
      Illegal

let accept give receive target st player_name =
  let rec can_pay_claim st = function
    | [] -> true
    | (r,n)::t -> if List.assoc r
        (get_player_from_name player_name st).resources >= n then
        can_pay_claim st t else false in
  if can_pay_claim st give then
    let rec transaction pay_or_gain st pname = function
      | [] -> st
      | (r,n)::t -> transaction pay_or_gain
                      (edit_player st
                         (increment_resource
                            (get_player_from_name pname st)
                            (if pay_or_gain then (-n) else n) r)) pname t in
    Legal (transaction false (transaction true
                                (transaction false
                                   (transaction true st player_name give)
                                   player_name receive)
                                target receive) target give)
  else Illegal

let resources_for_player st player_name =
  (get_player_from_name player_name st).resources

let should_harvest st tile =
  if tile.dienum = 0 || tile.dienum = 7
     || tile.dienum = 21 || tile.dienum = 31
     || tile.gridnum = st.robber || tile.resource = Desert then false
  else true

let get_player_with_highest_score st exclude =
  let rec get_player_score_list players acc =
    match players with
    | [] -> acc
    | (order, player)::t when player.name <> exclude ->
      get_player_score_list t ((player.score, player)::acc)
    | _::t -> get_player_score_list t acc
  in
  let player_score_list = get_player_score_list st.player_order [] in
  let sorted_player_score_list = List.rev
      (List.sort Pervasives.compare player_score_list) in
  snd (List.nth sorted_player_score_list 0)

(** [contains_gridnum location gridnum] is true if the tile list [location]
    contains the gridnum [gridnum] *)
let rec contains_gridnum location gridnum = match location with
  | [] -> false
  | h::t -> if h.gridnum = gridnum then true
    else contains_gridnum t gridnum

let has_city_or_settlement_by_tile st player gridnum =
  let buildings = get_buildings_of_player st player in
  let rec help buildings = match buildings with
    | [] -> false
    | h::t when h.build_type = Settlement || h.build_type = City ->
      if contains_gridnum h.location gridnum then true
      else help t
    | h::t -> help t
  in
  help buildings

let can_draw st =
  not st.has_drawn

let can_play_card st =
  not st.has_used_card

let ai_rates st name =
  let ports = on_ports [] (get_player_from_name name st) st.tiles
      (get_buildings st) (get_ports st)
  in
  get_rates ports
