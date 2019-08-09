type object_phrase = string list

type prompt_command =
  | Discard of (State.resource * int) list
  | Trade of bool
  | Robber of int
  | Steal of string

exception Illegal

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
    raises [Illegal] if [s] doesn't represent a resource*)
let resource_of_string s =
  match s with
  | "brick" -> State.Brick
  | "wood" -> State.Wood
  | "sheep" -> State.Sheep
  | "grain" -> State.Grain
  | "rock" -> State.Rock
  | _ -> raise Illegal

(** [discard_parser obj_phrase] is the (resource * int) list from obj_phrase
    required by a [Discard] type. raises Illegal if [obj_phrase] cannot be 
    parsed to a (resource * int) list *)
let discard_parser object_phrase =
  let rec resources_from_obj_phrase obj_phrase expecting_resources acc =
    match obj_phrase with
    | [] -> acc
    | h::t -> match acc with
      | (r_lst, int_lst) ->
        if expecting_resources then
          let resource = resource_of_string h in
          resources_from_obj_phrase t false (resource::r_lst, int_lst)
        else
          let intVal = try int_of_string h with Failure _ ->
            raise Illegal in
          resources_from_obj_phrase t true (r_lst, intVal::int_lst)
  in
  let rec merge_r_i_lst r_lst i_lst acc =
    if List.length r_lst <> List.length i_lst then raise Illegal
    else
      match r_lst with
      | [] -> acc
      | h::t -> match i_lst with
        | [] -> raise Illegal
        | h'::t' -> merge_r_i_lst t t' ((h, h')::acc)
  in
  let r_i_list_tuple = resources_from_obj_phrase object_phrase true ([],[]) in
  merge_r_i_lst (fst r_i_list_tuple) (snd r_i_list_tuple) []

(** [trade_parser obj_phrase] is the parsed object phrase for
    trade command *)
let trade_parser object_phrase =
  match first_non_empty object_phrase with
  | "accept" -> true
  | "reject" -> false
  | _ -> raise Illegal

(** [robber_parser obj_phrase] is the parsed object phrase for
    robber command *)
let robber_parser object_phrase =
  let gridnum_string = first_non_empty object_phrase in
  try int_of_string gridnum_string with Failure _ -> raise Illegal

(** [steal_parser obj_phrase] is the parsed object phrase for
    steal command *)
let steal_parser object_phrase = first_non_empty object_phrase


let parse str =
  let str = String.lowercase_ascii str in
  let word_lst = String.split_on_char ' ' str  in
  let object_phrase = rem_first_word word_lst [] in
  let word = first_non_empty word_lst in
  match word with
  | "discard" -> Discard (discard_parser object_phrase)
  | "trade" -> Trade (trade_parser object_phrase)
  | "robber" -> Robber (robber_parser object_phrase)
  | "steal" -> Steal (steal_parser object_phrase)
  | _ -> raise Illegal
