open State

let choose_color players =
  let colors = [State.Red; State.Green; State.Yellow; State.Blue;
                State.Magenta; State.Cyan] in
  let rec taken_colors players acc = match players with
    | [] -> acc
    | h::t -> taken_colors t (h.color::acc)
  in
  let rec available_colors taken colors acc = match colors with
    | [] -> acc
    | h::t when List.mem h taken -> available_colors taken t acc
    | h::t -> available_colors taken t (h::acc)
  in
  let available_colors = available_colors (taken_colors players []) colors []
  in
  let generator = QCheck.Gen.int_range 0
      (List.length available_colors - 1) in
  let index = QCheck.Gen.generate1 generator in
  List.nth available_colors index

let choose_name players =
  let names = ["ocaml"; "java"; "cplusplus"; "php"; "swift";
               "python"; "coq"] in
  let rec taken_names players acc = match players with
    | [] -> acc
    | h::t -> taken_names t (h.name::acc)
  in
  let rec available_names taken names acc = match names with
    | [] -> acc
    | h::t when List.mem h taken -> available_names taken t acc
    | h::t -> available_names taken t (h::acc)
  in
  let available_names = available_names (taken_names players []) names []
  in
  let generator = QCheck.Gen.int_range 0
      (List.length available_names - 1) in
  let index = QCheck.Gen.generate1 generator in
  List.nth available_names index

(** [initial_resource_expecation_lst] is a resource * int list of
    resources and their expecations set to 0 *)
let initial_resource_expecation_lst = [(Brick, 0); (Wood, 0); (Sheep, 0);
                                       (Grain, 0); (Rock, 0)]
(** [probability_from_dienum dienum] is the probability that a dienum would
    be selected by a diceroll. represented as 100 times of the probability. *)
let rec probability_from_dienum dienum = match dienum with
  | 2 -> 3
  | 12 -> 3
  | 3 -> 6
  | 11 -> 6
  | 4 -> 8
  | 10 -> 8
  | 5 -> 11
  | 9 -> 11
  | 6 -> 14
  | 8 -> 14
  | 7 -> 17
  | _ -> 0

let rec from i j l = if i>j then l else from i (j-1) (j::l)

let alist = from 0 41 []

let generate_locations_from_A a =
  if (a/7) mod 2 = 0 then
    if a mod 7 < 6 then
      ([a;a+1;a+8], [a;a+7;a+8])
    else
      ([],[])
  else
  if a mod 7 < 6 then
    if a mod 7 > 0 then
      ([a;a+1;a+7], [a;a+6;a+7])
    else
    if a = 21 then
      ([21;22;28],[])
    else
      ([],[])
  else
    ([],[])

let rate_location = function
  | a::b::c::[] -> begin
      let adie = a.dienum in
      let bdie = b.dienum in
      let cdie = c.dienum in
      if adie = 7 then
        abs (7 - bdie) + abs (7 - cdie) + 7
      else
      if bdie = 7 then
        abs (7 - adie) + abs (7 - cdie) + 7
      else
      if cdie = 7 then
        abs (7 - adie) + abs (7 - bdie) + 7
      else
        abs (7 - adie) + abs (7 - bdie) + abs (7 - cdie)
    end
  | _ -> failwith "misused"

let rec match_spots_with_rank st acc = function
  |  [] -> acc
  | a::t -> begin
      let (x,y) = generate_locations_from_A a in
      if x = [] then
        if y = [] then
          match_spots_with_rank st acc t
        else
          let ytiles = State.tile_list_from_gridnum_list (get_tiles st) y in
          let yrating = rate_location ytiles in
          match_spots_with_rank st ((yrating, y)::acc) t
      else
      if y = [] then
        let xtiles = State.tile_list_from_gridnum_list (get_tiles st) x in
        let xrating = rate_location xtiles in
        match_spots_with_rank st ((xrating, x)::acc) t
      else
        let xtiles = State.tile_list_from_gridnum_list (get_tiles st) x in
        let xrating = rate_location xtiles in
        let ytiles = State.tile_list_from_gridnum_list (get_tiles st) y in
        let yrating = rate_location ytiles in
        match_spots_with_rank st ((yrating, y)::(xrating, x)::acc) t
    end

let compare_rankings (rate1, _) (rate2, _) = Pervasives.compare rate2 rate1

let prioritize_locations st =
  List.sort compare_rankings (match_spots_with_rank st [] alist)

let can_use st name (rating,loc) =
  match State.init_placements Settlement
          (tile_list_from_gridnum_list (get_tiles st) loc) st name with
  | Legal _ -> true
  | _ -> false

let rec remove_taken st name acc = function
  | [] -> acc
  | h::t -> if can_use st name h then remove_taken st name (h::acc) t else
      remove_taken st name acc t

let die_probs = [
  (2,1.0/.36.0);
  (3,2.0/.36.0);
  (4,3.0/.36.0);
  (5,4.0/.36.0);
  (6,5.0/.36.0);
  (7,6.0/.36.0);
  (8,5.0/.36.0);
  (9,4.0/.36.0);
  (10,3.0/.36.0);
  (11,2.0/.36.0);
  (12,1.0/.36.0);
  (21,0.0);
  (31,0.0)
]

let rec get_prob die prob_assoc_lst =
  match prob_assoc_lst with
  | [] -> failwith "This should never fail"
  | h::t -> if fst h = die then snd h else get_prob die t

type res_list = {player : string; brick : float; wood : float;
                 sheep : float; rock : float; grain : float}

let rec add_res_for_tiles incr tiles player_res =
  match tiles with
  | [] -> player_res
  | h::t -> add_res_for_tiles incr t (match h.resource with
      | Brick -> {player_res with
                  brick = player_res.brick +.
                          incr*.(get_prob h.dienum die_probs)}
      | Sheep -> {player_res with
                  sheep = player_res.sheep +.
                          incr*.(get_prob h.dienum die_probs)}
      | Wood -> {player_res with wood =
                                   player_res.wood +.
                                   incr*.(get_prob h.dienum die_probs)}
      | Rock -> {player_res with rock =
                                   player_res.rock +.
                                   incr*.(get_prob h.dienum die_probs)}
      | Grain -> {player_res with grain =
                                    player_res.grain +.
                                    incr*.(get_prob h.dienum die_probs)}
      | _ -> player_res)

let rec gen_player_res_prod (player : State.player)
    (buildings : State.building list) (player_res : res_list) =
  match buildings with
  | [] -> player_res
  | h::t ->
    (if (h.player.name = player.name)
     then
       (match h.build_type with
        | Settlement -> gen_player_res_prod player t
                          (add_res_for_tiles 1.0 h.location player_res)
        | City -> gen_player_res_prod player t
                    (add_res_for_tiles 2.0 h.location player_res)
        | _ -> gen_player_res_prod player t player_res
       ) else gen_player_res_prod player t player_res)

let rec gen_expected_res_per_turn st players player_res_lsts =
  match players with
  | [] -> player_res_lsts
  | h::t -> gen_expected_res_per_turn st t
              (gen_player_res_prod h (State.get_buildings st)
                 {player = h.name; brick = 0.0; wood = 0.0;
                  sheep = 0.0; rock = 0.0; grain = 0.0}::player_res_lsts)

let rec get_player_expectations player player_res_lsts =
  match player_res_lsts with
  | [] -> failwith "Player not found"
  | h::t -> if h.player = player.name then h
    else get_player_expectations player t

let rec determine_goals st name =
  let potential =prioritize_locations st in
  let usable_spots = remove_taken st name [] potential in
  usable_spots

let rec determine_player_best_moves st players player_goal_lsts =
  match players with
  | [] -> player_goal_lsts
  | h::t -> determine_player_best_moves st t
              ((h,determine_goals st h.name)::player_goal_lsts)

let rec min_dist_to_targ st target
    player immutable_bs buildings (min_dist : int) =
  match buildings with
  | [] -> min_dist
  | h::t -> let dist,_ =
              (State.shortest_path (State.grab_gridnums h.location)
                 target [(State.grab_gridnums h.location)]
                 (State.get_tiles st) immutable_bs
                 1 player 7) in
    (if (h.player.name = player.name) then (match h.build_type with
         | Road -> if dist < min_dist
           then min_dist_to_targ st target player immutable_bs t dist
           else min_dist_to_targ st target player immutable_bs t min_dist
         | _ -> min_dist_to_targ st target player immutable_bs t min_dist
       ) else min_dist_to_targ st target player immutable_bs t min_dist)

(** [string_of_resource res] is the string corresponsding to the resource
    [res] *)
let string_of_resource (resource : State.resource) =
  match resource with
  | Brick -> "Brick"
  | Wood -> "Wood"
  | Sheep -> "Sheep"
  | Grain -> "Grain"
  | Rock -> "Rock"
  | _ -> "Invalid resource"

let get_non_zero_prods res_prod acc =
  acc := res_prod.brick +. !acc;
  acc := res_prod.wood +. !acc;
  acc := res_prod.grain +. !acc;
  acc := res_prod.sheep +. !acc;
  acc := res_prod.rock +. !acc;
  !acc

let compute_up_prods (res_prod : res_list) upd =
  ((if res_prod.brick = 0.0 then res_prod.brick +. upd else res_prod.brick),
   (if res_prod.wood = 0.0 then res_prod.wood +. upd else res_prod.wood),
   (if res_prod.sheep = 0.0 then res_prod.sheep +. upd else res_prod.sheep),
   (if res_prod.rock = 0.0 then res_prod.rock +. upd else res_prod.rock),
   (if res_prod.grain = 0.0 then res_prod.grain +. upd else res_prod.grain))

let expected_num_turns_to_reach_goal st goal player buildings player_res_lsts =
  let roads = float_of_int(min_dist_to_targ st goal player buildings
                             (State.get_roads [] buildings) 999) in
  let (wood,brick) = (float_of_int(List.assoc Brick player.resources),
                      float_of_int(List.assoc Wood player.resources)) in
  let (grain,sheep) = (float_of_int(List.assoc Grain player.resources),
                       float_of_int(List.assoc Sheep player.resources)) in
  let res_prod = get_player_expectations player player_res_lsts in
  let float_ref = ref 0.0 in
  let zero_prod = (get_non_zero_prods res_prod (float_ref))/.4.0 in
  match (compute_up_prods res_prod zero_prod) with
  | (brick_prod, wood_prod, sheep_prod, rock_prod, grain_prod) ->
    max (max((roads -. wood)/.wood_prod) (roads -. sheep)/.sheep_prod)
      (max((roads -. brick)/.brick_prod) (roads -. grain)/.grain_prod)

let pick_trade_res st goal player buildings player_res_lsts =
  let roads = float_of_int(min_dist_to_targ st goal player buildings
                             (State.get_roads [] buildings) 999) in
  let (wood,brick) = (float_of_int(List.assoc Brick player.resources),
                      float_of_int(List.assoc Wood player.resources)) in
  let (grain,sheep) = (float_of_int(List.assoc Grain player.resources),
                       float_of_int(List.assoc Sheep player.resources)) in
  let res_prod = get_player_expectations player player_res_lsts in
  let brick_prod = res_prod.brick in
  let wood_prod = res_prod.wood in
  let grain_prod = res_prod.grain in
  let sheep_prod = res_prod.sheep in
  if (roads +. 1.0 -. brick > 0.0) && brick_prod = 0.0 then Some Brick else
  if (roads +. 1.0 -. wood > 0.0) && wood_prod = 0.0 then Some Wood else
  if (1.0 -. grain > 0.0) && grain_prod = 0.0 then Some Grain else
  if (1.0 -. sheep > 0.0) && sheep_prod = 0.0 then Some Sheep else None

let trade st goal player =
  let player_res_lsts =
    gen_expected_res_per_turn st (State.get_players st) [] in
  let opt =
    pick_trade_res st goal player (State.get_buildings st) player_res_lsts in
  match opt with
  | None -> ("", false)
  | Some res ->
    let res_prod = get_player_expectations player player_res_lsts in
    let brick_prod = res_prod.brick in
    let wood_prod = res_prod.wood in
    let grain_prod = res_prod.grain in
    let sheep_prod = res_prod.sheep in
    let rock_prod = res_prod.rock in
    let res_str = string_of_resource res in
    if rock_prod > 0.0 && (List.assoc Rock player.resources) >=
                          (List.assoc Rock (State.ai_rates st player.name))
    then ("trade bank offer rock "^string_of_int((List.assoc Rock (State.ai_rates st player.name)))^" receive "^res_str^" 1", true) else
    if sheep_prod > 0.0 && (List.assoc Sheep player.resources) >=
                           (List.assoc Sheep (State.ai_rates st player.name))
    then ("trade bank offer sheep "^string_of_int((List.assoc Sheep (State.ai_rates st player.name)))^" receive "^res_str^" 1", true) else
    if grain_prod > 0.0 && (List.assoc Grain player.resources) >=
                           (List.assoc Grain (State.ai_rates st player.name))
    then ("trade bank offer grain "^string_of_int((List.assoc Grain (State.ai_rates st player.name)))^" receive "^res_str^" 1", true) else
    if brick_prod > 0.0 && (List.assoc Brick player.resources) >=
                           (List.assoc Brick (State.ai_rates st player.name))
    then ("trade bank offer brick "^string_of_int((List.assoc Brick (State.ai_rates st player.name)))^" receive "^res_str^" 1", true) else
    if wood_prod > 0.0 && (List.assoc Wood player.resources) >=
                          (List.assoc Wood (State.ai_rates st player.name))
    then ("trade bank offer wood "^string_of_int((List.assoc Wood (State.ai_rates st player.name)))^" receive "^res_str^" 1", true)
    else ("", false)

let trade_cities st player =
  let player_res_lsts =
    gen_expected_res_per_turn st (State.get_players st) [] in
  let res_prod = get_player_expectations player player_res_lsts in
  let brick_prod = res_prod.brick in
  let wood_prod = res_prod.wood in
  let sheep_prod = res_prod.sheep in
  let res = if List.assoc Rock player.resources >= 3 then Grain
    else Rock in
  let res_str = string_of_resource res in
  if sheep_prod > 0.0 && (List.assoc Sheep player.resources) >=
                         (List.assoc Sheep (State.ai_rates st player.name))
  then ("trade bank offer sheep "^string_of_int((List.assoc Sheep (State.ai_rates st player.name)))^" receive "^res_str^" 1", true)
  else
  if brick_prod > 0.0 && (List.assoc Brick player.resources) >=
                         (List.assoc Brick (State.ai_rates st player.name))
  then ("trade bank offer brick "^string_of_int((List.assoc Brick (State.ai_rates st player.name)))^" receive "^res_str^" 1", true)
  else
  if wood_prod > 0.0 && (List.assoc Wood player.resources) >=
                        (List.assoc Wood (State.ai_rates st player.name))
  then ("trade bank offer wood "^string_of_int((List.assoc Wood (State.ai_rates st player.name)))^" receive "^res_str^" 1", true)
  else ("", false)


let rec get_goal_winner st players goal buildings player_res_lsts leader =
  match players with
  | [] -> leader
  | h::t ->
    if expected_num_turns_to_reach_goal st goal h buildings player_res_lsts
       < expected_num_turns_to_reach_goal st goal leader
         buildings player_res_lsts
    then get_goal_winner st t goal buildings player_res_lsts h
    else get_goal_winner st t goal buildings player_res_lsts leader

let rec loc_to_string loc acc =
  match loc with
  | [] -> acc
  | h::t -> loc_to_string t ((string_of_int h)^" "^acc)

let rec determine_goal_helper st player goals res_lsts =
  match goals with
  | [] -> None
  | (_,loc)::t ->
    if (get_goal_winner st (State.get_players st) loc
          (State.get_buildings st) res_lsts player).name =
       player.name then Some loc else
      determine_goal_helper st player t res_lsts

let determine_goal st player =
  let goals = determine_goals st player.name in
  let res_lsts = gen_expected_res_per_turn st (State.get_players st)
      [] in
  determine_goal_helper st player goals res_lsts

let get_shortest_path st loc =
  let player = State.get_current_player st in
  let buildings = State.get_buildings_of_player st
      player.name in
  let rec shorty len path st loc = function
    | [] -> path
    | h::t -> if h.build_type = Settlement || h.build_type = City
      then
        let (temp_length, temp_path) =
          State.path_from_settlement_to_vertex
            (State.grab_gridnums h.location) player.name loc st 7 in
        if temp_length < len then
          shorty temp_length temp_path st loc t
        else
          shorty len path st loc t
      else
        shorty len path st loc t
  in
  shorty 99 [] st loc buildings

let rec extract_unbuilt_roads st path = match path with
  | [] -> path
  | h::t -> (if State.spot_taken (tile_list_from_gridnum_list
                                    (State.get_tiles st) h) (get_buildings st)
             then extract_unbuilt_roads st t
             else h::(extract_unbuilt_roads st t))

let place_initial_settlement st name =
  let spots = prioritize_locations st in
  let usable_spots = remove_taken st name [] spots in
  match usable_spots with
  | [] -> failwith "uh there should never be no usable spots"
  | (_,loc)::t -> begin
      match loc with
      | a::b::c::[] -> begin
          "build settlement " ^ (string_of_int a) ^ " " ^ (string_of_int b)
          ^ " " ^ (string_of_int c)
        end
      | _ -> failwith "you done goofed"
    end

let rec keep_in_range st loc name acc range = function
  | [] -> acc
  | (_,l)::t -> let len, path = State.path_from_settlement_to_vertex loc name l
                    st range in
    if len < range then keep_in_range st loc name ((path,l)::acc)
        range t else keep_in_range st loc name acc range t

let place_initial_road st name settlement_loc =
  let gol = determine_goal st (State.get_player_from_name name st) in
  match gol with
  | None -> (match settlement_loc with
      | a::b::c::[] ->
        "build road " ^ (string_of_int a) ^ " " ^ (string_of_int b)
      | _ -> failwith "uh, built a settlement incorrectly?")
  | Some goal ->
    let (_, path) =
      State.path_from_settlement_to_vertex settlement_loc name
        goal st 7 in
    let roads = extract_unbuilt_roads st path in
    match roads with
    | [] -> (match settlement_loc with
        | a::b::c::[] ->
          "build road " ^ (string_of_int a) ^ " " ^ (string_of_int b)
        | _ -> failwith "uh, built a settlement incorrectly?")
    | h::t -> (match h with
        | a::b::[] ->
          "build road " ^ (string_of_int a) ^ " " ^ (string_of_int b)
        | _ -> failwith "uh, built a settlement incorrectly?")

(** [resource_expectation_from_tiles st build_type tiles acc] is the updated
    resource expecation list with [tiles], given the [build_type] and
    the initial resource expecation list [acc]. For example, if a city is built
    by the tile, then the resource expecation would be incremented by
    2 * probability of diceroll equal to the dienum of that tile. *)
let rec resource_expectation_from_tiles st build_type tiles acc =
  match tiles with
  | [] -> acc
  | h::t when build_type = City && State.should_harvest st h ->
    let current_resource_expectation = List.assoc h.resource acc in
    let updated_acc = (h.resource,
                       current_resource_expectation +
                       2 *
                       (probability_from_dienum h.dienum))
                      ::(List.remove_assoc h.resource acc) in
    resource_expectation_from_tiles st build_type t updated_acc
  | h::t when build_type = Settlement && State.should_harvest st h ->
    let current_resource_expectation = List.assoc h.resource acc in
    let updated_acc = (h.resource,
                       current_resource_expectation +
                       (probability_from_dienum h.dienum))
                      ::(List.remove_assoc h.resource acc) in
    resource_expectation_from_tiles st build_type t updated_acc
  | h::t -> resource_expectation_from_tiles st build_type t acc

(** [get_resource_expectation st player] is the resource expecation list
    for the player [player] at state [st]. The higher the expecation for
    a resource is, the more likely the player is going to receive resource
    of that type during a turn. *)
let get_resource_expectation st player =
  let player_buildings = State.get_buildings_of_player st player.name in
  let rec resource_expectation_help buildings acc =
    match buildings with
    | [] -> acc
    | h::t -> let updated_acc =
                resource_expectation_from_tiles st h.build_type h.location acc
      in
      resource_expectation_help t updated_acc
  in
  resource_expectation_help player_buildings initial_resource_expecation_lst

(** [select_resources_to_keep exclude ordered_expectation
    order_expectation_iterating owned count current_index init] will select
    [count]
    amount of resources to keep with the following rule:
    starting from the least expectecd resource in [ordered_expectation],
    if the resource is not excluded and the player ownes enough to keep one
    more
    of this kind of resource, keep 1 more of this resource, then proceed to
    the second least expected resource in [ordered_expectation] with the same
    rule.
    If the entirely [ordered_expection] list is gone over and we haven't yet
    selected [count] amount of resources to keep, start the process again from
    the least expected resource in [ordered_expectation].
    Requires: the sum of all ints in [init] and [count] should be less than or
    equal to the sum of all ints in [owned] *)
let rec select_resources_to_keep
    exclude
    ordered_expectation
    ordered_expectation_iterating
    owned
    count
    current_index
    init =
  if current_index = count then init
  else
    match ordered_expectation_iterating with
    | [] -> select_resources_to_keep exclude
              ordered_expectation ordered_expectation owned
              count current_index init
    | (resource, _)::t when List.mem resource exclude ->
      select_resources_to_keep exclude ordered_expectation
        t owned count current_index init
    | (resource, _)::t ->
      if List.assoc resource init + 1 > List.assoc resource owned then
        select_resources_to_keep exclude
          ordered_expectation t owned count current_index init
      else
        let updated_acc =
          (resource, List.assoc resource init + 1)
          ::(List.remove_assoc resource init) in
        select_resources_to_keep exclude
          ordered_expectation t owned count (current_index + 1) updated_acc

(** [get_rem_list keep_list owned acc] is the association list of owned -
    keep_list *)
let rec get_rem_list keep_list owned acc =
  match owned with
  | [] -> acc
  | (resource, amount)::t ->
    let updated_acc = (resource, amount - List.assoc resource keep_list)
                      ::(List.remove_assoc resource acc) in
    get_rem_list keep_list t updated_acc

(** [discard_command_from_rem_list rem_list acc] converts contents in the
    [rem_list] into a discard command string *)
let rec discard_command_from_rem_list rem_list acc =
  match rem_list with
  | [] -> acc
  | (resource, count)::t when count > 0 ->
    let updated_acc = acc ^ " " ^ string_of_resource resource ^
                      " " ^ string_of_int count in
    discard_command_from_rem_list t updated_acc
  | h::t -> discard_command_from_rem_list t acc

(** [sum_r_i_list list acc] is the sum of the resource * int list [list] *)
let rec sum_r_i_list list acc = match list with
  | [] -> acc
  | (resource, count)::t -> sum_r_i_list t (acc + count)

let discard st player amount =
  let compare_r_i (resource1, amount1) (resource2, amount2) =
    if amount1 < amount2 then -1
    else if amount1 = amount2 then 0
    else 1
  in
  let ordered_expectation_list = List.sort
      compare_r_i (get_resource_expectation st player) in
  let total_resource_count = sum_r_i_list player.resources 0 in
  (* Prioritize city building. If the player has enough resources to build a
     city, then keep at least that amount, then select other rousources with
     lowest expectation to keep. *)
  if List.assoc Rock player.resources >= 3
  && List.assoc Grain player.resources >= 2
  && total_resource_count - amount >= 5 then
    let keep_list = [(Brick, 0); (Wood, 0); (Sheep, 0);
                     (Grain, 2); (Rock, 3)] in
    let additional_amount_to_keep = total_resource_count - amount - 5 in
    let keep_list = select_resources_to_keep
        [Grain; Rock] ordered_expectation_list
        ordered_expectation_list player.resources
        additional_amount_to_keep 0 keep_list in
    let rem_list = get_rem_list keep_list player.resources [] in
    discard_command_from_rem_list rem_list "discard"
    (* Prioritize settlement building secondly. The player will keep at least
       an amount enough to build one settlement, and then select the rest
       resources to keep based on expectation. *)
  else if List.assoc Brick player.resources >= 1
       && List.assoc Wood player.resources >= 1
       && List.assoc Sheep player.resources >= 1
       && List.assoc Grain player.resources >= 1
       && total_resource_count - amount >= 4 then
    let keep_list = [(Brick, 1); (Wood, 1); (Sheep, 1);
                     (Grain, 1); (Rock, 0)] in
    let additional_amount_to_keep = total_resource_count - amount - 4 in
    let keep_list = select_resources_to_keep
        [] ordered_expectation_list
        ordered_expectation_list player.resources
        additional_amount_to_keep 0 keep_list in
    let rem_list = get_rem_list keep_list player.resources [] in
    discard_command_from_rem_list rem_list "discard"
  else
    let keep_list = [(Brick, 0); (Wood, 0); (Sheep, 0);
                     (Grain, 0); (Rock, 0)] in
    let additional_amount_to_keep = total_resource_count - amount in
    let keep_list = select_resources_to_keep
        [] ordered_expectation_list
        ordered_expectation_list player.resources
        additional_amount_to_keep 0 keep_list in
    let rem_list = get_rem_list keep_list player.resources [] in
    discard_command_from_rem_list rem_list "discard"

let start_turn st =
  if not(get_turn_status st) then "start" else ""

let upgradeable_spots st =
  let bldngs = State.get_buildings_of_player st (State.get_current_player
                                                   st).name in
  let rec extract_settlements acc = function
    | [] -> acc
    | h::t -> if h.build_type = Settlement then extract_settlements (h::acc) t
      else extract_settlements acc t in
  List.map (fun b -> (State.grab_gridnums b.location))
    (extract_settlements [] bldngs)

let find_best_upgrade_spot st =
  let sorted_locs = List.map (fun (a,b) -> b) (prioritize_locations st) in
  let rec best_spot potentials = function
    | [] -> None
    | h::t -> if List.mem h potentials then Some h
      else best_spot potentials t
  in
  best_spot (upgradeable_spots st) (sorted_locs)

let rec ai_trade st =
  let player = State.get_current_player st in
  let cutoff = (State.ai_rates st player.name) in
  if (List.assoc Brick player.resources >= List.assoc Brick cutoff
      || List.assoc Grain player.resources >= List.assoc Grain cutoff
      || List.assoc Sheep player.resources >= List.assoc Sheep cutoff
      || List.assoc Rock player.resources >= List.assoc Rock cutoff
      || List.assoc Wood player.resources >= List.assoc Wood cutoff)
  then (
    match determine_goal st player with
    | None ->
      let (trade_comm, should_trade) = trade_cities st player
      in
      if should_trade then (trade_comm, should_trade) else "", false
    | Some loc ->
      let (trade_comm, should_trade) = trade st loc player in
      if should_trade then (trade_comm, should_trade) else ("", false))
  else ("", false)

(** [can_build_anything st player] is true if the player can build anything
    and false otherwise. *)
let can_build_anything st player =
  ((List.assoc Brick player.resources >= 1)
   && (List.assoc Wood player.resources >= 1))
  || ((List.assoc Brick player.resources >= 1)
      && (List.assoc Wood player.resources >= 1)
      && (List.assoc Grain player.resources >= 1)
      && (List.assoc Sheep player.resources >= 1))
  || ((List.assoc Rock player.resources >= 3)
      && (List.assoc Grain player.resources >= 2))

(** [build_city st player] attempts to build a city for AI player [player] if 
    possible. *)
let build_city st player =
  let player = State.get_current_player st in
  if List.assoc Rock player.resources >= 3
  && List.assoc Grain player.resources >= 2 then
    match find_best_upgrade_spot st with
    | None -> ("", false)
    | Some l ->
      (match l with
       | a::b::c::[] ->
         ("build city " ^ (string_of_int a) ^ " " ^
          (string_of_int b) ^ " " ^ (string_of_int c), true)
       | _ -> failwith "invalid location thanks liam")
  else ("", false)

(** [build st player] attempts to build roads, cities, and settlements for 
    player [player]. *)
let rec build st player =
  if can_build_anything st player then (
    match determine_goal st player with
    | None -> "", false
    | Some loc -> begin
        let roads_to_build = get_shortest_path st loc
                             |> extract_unbuilt_roads st in
        if List.assoc Brick player.resources >= 1
        && List.assoc Wood player.resources >= 1 then
          match roads_to_build with
          | [] -> (
              if List.assoc Brick player.resources >= 1
              && List.assoc Wood player.resources >= 1
              && List.assoc Sheep player.resources >= 1
              && List.assoc Grain player.resources >= 1 then
                match loc with
                | a::b::c::[] ->
                  "build settlement " ^ (string_of_int a)
                  ^ " " ^ (string_of_int b) ^
                  " " ^ (string_of_int c), true
                | _ -> failwith "invalid location thanks liam"
              else
                "", false
            )
          | path::t -> (match path with
              | [] -> failwith "improper path given"
              | a::b::[] ->
                "build road " ^ (string_of_int a) ^ " "
                ^ (string_of_int b), true
              | _ -> failwith "uh, built a settlement incorrectly?")
        else
          "", false
      end)
  else "", false


(** [tile_expectation_from_loc build_type tiles acc] is the expectation of
    amount of resources generated for the tiles in [tiles], based on whether
    the tiles are around what types of buildings specified by [build_type] *)
let rec tile_expectation_from_loc build_type tiles acc =
  match tiles with
  | [] -> acc
  | h::t when build_type = Settlement ->
    if List.mem_assoc h.gridnum acc then
      let updated_acc = (
        h.gridnum, List.assoc h.gridnum acc
                   + probability_from_dienum h.dienum)
        ::(List.remove_assoc h.gridnum acc) in
      tile_expectation_from_loc build_type t updated_acc
    else
      let updated_acc = (h.gridnum, probability_from_dienum h.dienum)::acc in
      tile_expectation_from_loc build_type t updated_acc
  | h::t when build_type = City ->
    if List.mem_assoc h.gridnum acc then
      let updated_acc = (
        h.gridnum, List.assoc h.gridnum acc
                   + 2 * probability_from_dienum h.dienum)
        ::(List.remove_assoc h.gridnum acc) in
      tile_expectation_from_loc build_type t updated_acc
    else
      let updated_acc = (h.gridnum, 2 * probability_from_dienum h.dienum)::acc
      in
      tile_expectation_from_loc build_type t updated_acc
  | h::t -> tile_expectation_from_loc build_type t acc

(** [get_tile_expectation st player] is the expectation of the amount of
    resources generated for tiles that are beside [player]'s
    resource-harvesting
    buildings *)
let get_tile_expectation st player =
  let rec tile_expecation_from_buildings buildings acc =
    match buildings with
    | [] -> acc
    | h::t ->
      let updated_acc = tile_expectation_from_loc h.build_type h.location acc
      in
      tile_expecation_from_buildings t updated_acc
  in
  tile_expecation_from_buildings
    (State.get_buildings_of_player st player.name) []

(** [rem_current_robber_tile tile_expectation robber_gridnum acc] removes the
    robber gridnum key from [tile_expectation] *)
let rec rem_current_robber_tile tile_expectation robber_gridnum acc =
  match tile_expectation with
  | [] -> acc
  | (gridnum, value)::t -> if gridnum = robber_gridnum then
      rem_current_robber_tile t robber_gridnum acc
    else
      rem_current_robber_tile t robber_gridnum ((gridnum, value)::acc)

(** [rem_tiles_of_player st player tile_expectation acc] removes the player
    [player]'s tiles from [tile_expectations]*)
let rec rem_tiles_of_player st player tile_expectation acc =
  match tile_expectation with
  | [] -> acc
  | (gridnum, value)::t ->
    if State.has_city_or_settlement_by_tile st player.name gridnum then
      rem_tiles_of_player st player t acc
    else
      rem_tiles_of_player st player t ((gridnum, value)::acc)

(** [move_robber st] is used by the AI player to move the robber. *)
let move_robber st =
  let compare_gridnum_expecation (gridnum1, exp1) (gridnum2, exp2) =
    if exp1 < exp2 then -1
    else if exp1 = exp2 then 0
    else 1
  in
  let best_player = State.get_player_with_highest_score st
      (State.get_current_player st).name in
  let exp_lst = (get_tile_expectation st best_player)  in
  let exp_lst_rem_current_robber = rem_current_robber_tile exp_lst
      (State.get_robber_tile st) [] in
  let exp_lst_rem_player_tiles = rem_tiles_of_player st
      (State.get_current_player st) exp_lst_rem_current_robber [] in
  let sorted_exp_lst = if exp_lst_rem_player_tiles <> [] then
      List.sort compare_gridnum_expecation
        exp_lst_rem_player_tiles |> List.rev
    else
      List.sort compare_gridnum_expecation
        exp_lst_rem_current_robber |> List.rev
  in
  "robber " ^ string_of_int (fst (List.nth sorted_exp_lst 0))

(** [steal st players_to_steal] forms a command that steals from a player. *)
let steal st players_to_steal =
  let best_player = State.get_player_with_highest_score st
      (State.get_current_player st).name in
  if List.mem best_player.name players_to_steal = false then
    "steal " ^ (List.nth players_to_steal 0)
  else
    "steal " ^ best_player.name

(** [trade st give receive player] handles the logic for whether or not an AI
    should accept a trade initiated with it. *)
let trade st give receive player =
  let exp = get_resource_expectation st player in
  let rec expectation_for_gaining amounts expectation acc =
    match amounts with
    | [] -> acc
    | (resource, amount)::t when amount <> 0 ->
      let updated_acc = acc +.
                        float_of_int (List.assoc resource expectation)
                        /. (float_of_int amount) in
      expectation_for_gaining t expectation updated_acc
    | (resource, amount)::t ->
      let updated_acc = acc +.
                        float_of_int (List.assoc resource expectation)
                        /. 1.0 in
      expectation_for_gaining t expectation updated_acc
  in
  let exp_for_gaining_give = expectation_for_gaining give exp 0.0 in
  let exp_for_gaining_receive = expectation_for_gaining receive exp 0.0 in
  let give_sum = sum_r_i_list give 0 in
  let gain_sum = sum_r_i_list receive 0 in
  (* The AI will not consider the trade if the total it gets is less than
     the total it receives - 1 *)
  if give_sum - 1 <= gain_sum then
    if exp_for_gaining_give > exp_for_gaining_receive then
      "trade accept"
    else
      "trade reject"
  else "trade reject"


(** [find_max_resources state ] calculates the max resource that
    are in the players resource list. *)
let rec find_max_resources resources max =
  match resources with
  | [] -> max
  | (a,x)::t -> if x > snd max then find_max_resources t (a,x)
    else find_max_resources t max

(** [ai_find_most_resources state ] calculates the most resources that
    are in the
    best players resources, and chooses this resource for the monopoly card. *)
let ai_find_most_resources st =
  let player = State.get_player_with_highest_score st
      (State.get_current_player st).name in
  find_max_resources player.resources (Ocean, 0)

(** [ai_find_two_free_roads state ] finds two free roads to play to execute
    for the road build development card. *)
let ai_find_two_free_roads state =
  match determine_goal state (State.get_current_player state) with
  | None -> "", false
  | Some goal ->
    let roads_to_build = get_shortest_path state goal |>
                         extract_unbuilt_roads state in
    match roads_to_build with
    | a::b::t -> begin
        match a with
        | n1::n2::[] -> begin
            match b with
            | n3::n4::[] -> begin
                ("roadbuilding location " ^ (string_of_int n1) ^ " " ^
                 (string_of_int n2) ^ " location " ^ (string_of_int n3)
                 ^ " " ^ (string_of_int n4),true)
              end
            | _ -> failwith "invalid location"
          end
        | _ -> failwith "invalid location"
      end
    | _ -> ("", false)

(** [finds_least_resources resources least] calculates the least resources that
    are in the
    best players resources, and chooses this resource for the monopoly card. *)
let rec find_least_resources resources least =
  match resources with
  | [] -> least
  | (a,x)::t -> if x < snd least then
      find_least_resources t (a,x)
    else find_least_resources t least

(** [filter_resource_list lst resource acc ] filters players resources with
    selected resource list. *)
let rec filter_resource_list lst resource acc =
  match lst with
  | [] -> acc
  | (a,x)::t -> if resource = a then List.rev_append (List.rev acc) t
    else (filter_resource_list t resource ((a,x)::acc))

(** [ai_calculate_optimal_yop_pick state ] finds the optimal resource
    pick based on
    which two resources are least expected to be recieved
    at the end of the turn *)
let ai_calculate_optimal_yop_pick state =
  let player = (State.get_current_player state) in
  let expectation_list = get_resource_expectation state player in
  let first_resource = find_least_resources expectation_list (Ocean, 100) in
  let filtered_exp_list =
    filter_resource_list expectation_list (fst first_resource)  [] in
  let second_resource = find_least_resources filtered_exp_list (Ocean, 100) in
  string_of_resource (fst first_resource) ^
  " " ^ string_of_resource (fst second_resource)

(** [filter_rock_sheep_grain resources acc_lst] filters three resources
    to help check when the AI should draw cards *)
let rec filter_rock_sheep_grain resources acc_lst =
  match resources with
  | [] -> acc_lst
  | (a,x)::t -> if a = Rock || a = Sheep || a = Grain then
      filter_rock_sheep_grain t ((a,x)::acc_lst)
    else filter_rock_sheep_grain t acc_lst

(** [filter_threshold resources threshold] returns true if the resources are
    enough to draw dev cards *)
let rec filter_threshold resources threshold =
  match resources with
  | [] -> true
  | (a,x)::t -> if x >= threshold then filter_threshold t threshold else false

(** [ai_draw_card state] draws a card for the AI. *)
let ai_draw_card state =
  if State.can_draw state then
    let player = State.get_current_player state in
    let resource_list = State.resources_for_player state player.name in
    let filtered_list = filter_rock_sheep_grain resource_list [] in
    if filter_threshold filtered_list 1 then ("draw", true)
    else ("", false)
  else
    ("", false)

let ai_play_card state =
  (** select a random card to play *)
  let ai_player_cards = State.get_current_player_cards state in
  if(ai_player_cards <> [] && State.can_play_card state) then
    let temp =   List.nth ai_player_cards (Random.int
                                             (List.length ai_player_cards)) in
    let string_card = State.card_to_string temp in
    if string_card = "Knight" then ("knight", true)
    else if string_card = "Year of Plenty" then
      ("yearofplenty " ^
       ai_calculate_optimal_yop_pick state ^ "", true)
    else if string_card = "Monopoly" then
      let resource = fst (ai_find_most_resources state) in
      ("monopoly " ^  string_of_resource resource ^ "", true)
    else if string_card = "Road Builder" then
      ai_find_two_free_roads state
    else
      let vp_card =
        String.sub string_card 17 ((String.length string_card) - 17)
      in
      ("victorypoint" ^ vp_card ^ "", true)
  else ("", false)

let command st player =
  if State.get_turn_status st = false then
    "start"
  else
    let (city_cmd, should_city) = build_city st player in
    if should_city then city_cmd
    else
      let (trade_cmd, should_trade) = ai_trade st in
      if should_trade then trade_cmd
      else
        let (build_command, should_build) = build st player in
        if should_build then build_command
        else
          let (use_card_cmd, if_use_card) = ai_play_card st in
          if if_use_card then use_card_cmd
          else
            let (draw_cmd, if_draw) = ai_draw_card st in
            if if_draw then draw_cmd
            else
              "end"
