
type loc_phrase = int list
type object_phrase = string list

type command =
  | Start
  | Build of State.build_type * loc_phrase
  | Trade of string * (State.resource * int) list * (State.resource * int) list
  | Costs
  | Draw
  | KnightCard
  | VictoryPointCard of string
  | YearOfPlentyCard of (State.resource * State.resource)
  | MonopolyCard of State.resource
  | RoadBuildingCard of (loc_phrase * loc_phrase)
  | Resources
  | Scores
  | End
  | Help
  | Exit

exception Empty

exception Malformed

(** [first_non_empty str_lst] is the first non-empty string in [str_lst]. *)
let rec first_non_empty str_lst =
  match str_lst with
  | [] -> ""
  | h::t -> if h <> "" then h else first_non_empty t

(** [rem_first_word str_lst acc_lst] is the string list containing all
    non-empty strings of [str_lst], other than the first non-empty string.*)
let rec rem_first_word str_lst acc_lst =
  let rec remove_first_non_empty str_lst =
    match str_lst with
    | [] -> []
    | h::t -> if h <> "" then t else remove_first_non_empty t
  in
  match str_lst with
  | [] -> remove_first_non_empty (List.rev acc_lst)
  | h::t -> if h <> "" then (rem_first_word t (h::acc_lst))
    else rem_first_word t acc_lst


(** [resource_of_string s] is the resource that [s] represents.
    raises [Malformed] if [s] doesn't represent a resource*)
let resource_of_string s =
  match s with
  | "brick" -> State.Brick
  | "wood" -> State.Wood
  | "sheep" -> State.Sheep
  | "grain" -> State.Grain
  | "rock" -> State.Rock
  | _ -> raise Malformed

(** [year_of_plenty_parser obj_phrase] is the parsed object phrase for
    years of plenty command *)
let year_of_plenty_parser obj_phrase = 
  let fst_res = first_non_empty obj_phrase in 
  let obj_phrase = rem_first_word obj_phrase [] in 
  let snd_res = first_non_empty obj_phrase in 
  (resource_of_string fst_res, resource_of_string snd_res)

(** [monopoly_parser obj_phrase] is the parsed object phrase for
    monopoly command *)
let monopoly_parser obj_phrase = 
  let res = first_non_empty obj_phrase in 
  resource_of_string res

(** [road_building_parser obj_phrase] is the parsed object phrase for
    road building command *)
let road_building_parser obj_phrase = 
  if first_non_empty obj_phrase = "location" then
    let rec get_first_loc obj_phrase count acc = match obj_phrase with 
      | [] -> raise Malformed
      | h::t when h = "location" -> 
        if count <> 2 then 
          raise Malformed
        else
          (acc, t)
      | h::t -> if count > 2 then raise Malformed 
        else 
          let intVal = try int_of_string h with Failure _ -> raise Malformed in 
          get_first_loc t (count + 1) (intVal::acc)
    in 
    let rec get_snd_loc obj_phrase count acc = match obj_phrase with 
      | [] -> 
        if count <> 2 then 
          raise Malformed
        else
          acc 
      | h::t -> if count > 2 then raise Malformed 
        else 
          let intVal = try int_of_string h with Failure _ -> raise Malformed in 
          get_snd_loc t (count + 1) (intVal::acc)
    in
    let obj_phrase = rem_first_word obj_phrase [] in
    let first_loc, obj_phrase = get_first_loc obj_phrase 0 [] in 
    let snd_loc = get_snd_loc obj_phrase 0 [] in 
    first_loc,snd_loc
  else    
    raise Malformed

(** [trade_parser obj_phrase] is the parsed object phrase for
    trade command *)
let trade_parser obj_phrase = 
  let player_name = first_non_empty obj_phrase in 
  let obj_phrase = rem_first_word obj_phrase [] in 
  if first_non_empty obj_phrase = "offer" then 
    let obj_phrase = rem_first_word obj_phrase [] in 
    let rec merge_r_i_lst r_lst i_lst acc =
      if List.length r_lst <> List.length i_lst then raise Malformed
      else
        match r_lst with
        | [] -> acc
        | h::t -> match i_lst with
          | [] -> raise Malformed
          | h'::t' -> merge_r_i_lst t t' ((h, h')::acc)
    in
    let rec offer_from obj_phrase expecting_resources acc = 
      match obj_phrase with
      | [] -> raise Malformed
      | h::t when h = "receive" -> (acc, obj_phrase)
      | h::t -> match acc with
        | (r_lst, int_lst) ->
          if expecting_resources then
            let resource = resource_of_string h in
            offer_from t false (resource::r_lst, int_lst)
          else
            let intVal = try int_of_string h with Failure _ ->
              raise Malformed in
            offer_from t true (r_lst, intVal::int_lst)
    in
    let rec receive_from obj_phrase expecting_resources acc = 
      match obj_phrase with
      | [] -> acc
      | h::t -> match acc with
        | (r_lst, int_lst) ->
          if expecting_resources then
            let resource = resource_of_string h in
            receive_from t false (resource::r_lst, int_lst)
          else
            let intVal = try int_of_string h with Failure _ ->
              raise Malformed in
            receive_from t true (r_lst, intVal::int_lst)
    in
    let (offer_r_lst,offer_i_lst), obj_phrase = offer_from obj_phrase
        true ([], []) in 
    if first_non_empty obj_phrase = "receive" then
      let obj_phrase = rem_first_word obj_phrase [] in 
      let receive_r_lst, receive_i_lst = receive_from obj_phrase 
          true ([], []) in 
      player_name, merge_r_i_lst offer_r_lst offer_i_lst [], 
      merge_r_i_lst receive_r_lst receive_i_lst []
    else 
      raise Malformed
  else
    raise Malformed


(** [build_parser str_lst] is the tuple containing the building type
    and location specified within str_lst*)
let build_parser obj_phrase =
  let build = first_non_empty obj_phrase in
  let building = if build = "city" then State.City
    else if build = "road" then State.Road
    else if build = "settlement" then State.Settlement
    else if build = "ship" then State.Ship
    else if build = "wall" then State.Wall
    else raise Malformed
  in
  let loc = match rem_first_word obj_phrase [] with
    | a::b::c::[] as l -> (try List.map int_of_string l with Failure _ ->
        raise Malformed)
    | a::b::[] as l -> (try List.map int_of_string l with Failure _ ->
        raise Malformed)
    | _ -> raise Malformed
  in building, loc


let parse str =
  let str = String.lowercase_ascii str in
  let word_lst = String.split_on_char ' ' str  in
  let object_phrase = rem_first_word word_lst [] in
  let word = first_non_empty word_lst in
  match word with
  | "build" -> let build, loc = build_parser object_phrase
    in Build (build,loc)
  | "trade" -> (
      match trade_parser object_phrase with 
      | playername, offer_lst, receive_list ->
        Trade (playername, offer_lst, receive_list)
    )
  | "costs" -> if object_phrase = [] then
      Costs
    else
      raise Malformed
  | "exit" -> if object_phrase = [] then
      Exit
    else
      raise Malformed
  | "" -> raise Empty
  | "scores" -> if object_phrase = [] then
      Scores
    else
      raise Malformed
  | "resources" -> if object_phrase = [] then
      Resources
    else raise Malformed
  | "help" -> if object_phrase = [] then
      Help else
      raise Malformed
  | "start" -> if object_phrase = [] then
      Start else
      raise Malformed
  | "end" -> if object_phrase = [] then
      End else
      raise Malformed
  | "draw" -> if object_phrase = [] then
      Draw
    else
      raise Malformed
  | "knight" -> if object_phrase = [] then 
      KnightCard
    else
      raise Malformed
  | "victorypoint" -> 
    let card_name = first_non_empty object_phrase in 
    let object_phrase = rem_first_word object_phrase [] in 
    if object_phrase = [] then VictoryPointCard card_name 
    else raise Malformed
  | "yearofplenty" -> YearOfPlentyCard (year_of_plenty_parser object_phrase)
  | "monopoly" -> MonopolyCard (monopoly_parser object_phrase)
  | "roadbuilding" -> RoadBuildingCard (road_building_parser object_phrase)

  | _ -> raise Malformed
