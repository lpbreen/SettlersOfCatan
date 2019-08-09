(** Representation of the Game states. *)

(** The type [expansion] represents which expansion of the game has been
    selected. *)
type expansion = Basic

(** The type [resource] represents the resources of the game, which are
    associated with tiles, costs, and player inventories. *)
type resource = Brick | Wood | Sheep | Grain | Rock | Desert | Ocean

(** The type [color] corresponds to the color of a player's pieces. *)
type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

(** The type [progress] is the possible types for progress cards *)
type progress = YearOfPlenty | Monopoly | RoadBuilding

(** The type [card] is the possible types for bonus cards*)
type card = Knight of {played : bool}
          | Progress of {played : bool; progress : progress}
          | VP of {played : bool; pair : string * int}

(** The type [player] represents a player of the game. *)
type player = {name:string; score:int; color:color;
               resources: (resource * int) list; cards: card list; is_ai: bool}


(** The type [tile] represents a given hexagonal tile of the game board. *)
type tile = {resource:resource; dienum:int; gridnum:int}

(** The type [location] represents the location of a placement in terms of the
    tiles it touches. The tile list must be sorted in order of gridnum. *)
type location = tile list

(** The type [build_type] represents the type a building can be, according
    to the rules of the game. *)
type build_type = Settlement | Road | City | Ship | Wall

(** The type [building] represents a building on the game board. *)
type building = {player:player; location:location; build_type:build_type}

(** The type [placement] represents any object with a location, but may also
    include other properties. *)
type placement = Building of building | Port of resource * location

(** The type [t] represents the state of the game. *)
type t

(** The type [prompt] represents a prompt that could be send to main
    for further process *)
type prompt = Discard of (player * int) list * t
            | Trade of player * (resource * int) list * (resource * int) list
                       * t

(** The type [result] represents the result of an attempted command, legal
    with the new state if successful and illegal otherwise. *)
type result = Legal of t | Illegal | Prompt of prompt| Winner of player

(** [get tiles st] returns the list of board tiles in st. *)
val get_tiles : t -> tile list

(** [get_dimensions st] is the tuple of (width, height) of the board*)
val get_dimensions : t -> (int * int)

(** [get_placements st] is all the placements in the game *)
val get_placements : t -> placement list

(** [get_buildings st] is all the buildings in the game *)
val get_buildings : t -> building list

(** [get_current_player st] is the current player in the turn *)
val get_current_player : t -> player

(** [get_turn_status st] is the current status of turns. If the value is true,
    then a turn is ongoing, if not, then the game is expecting a start command.
*)
val get_turn_status : t -> bool

(** [get_buildings_of_player st pname] is the building list in which all
    buildings belong to player with name [pname] *)
val get_buildings_of_player : t -> string -> building list

(** [next_player st] returns the player whose turn should be next based on st.
*)
val next_player: t -> player

(** [init] is the suggested beginner game board, hard-coded for the demo. *)
val init : expansion -> player list -> (int * player) list -> t

(** [init_placements b loc st player] is Legal with the new state if the
    player is able
      to place the desired building [b] at specified location [l] in st,
      ignoring
      cost, and Illegal otherwise *)
val init_placements : build_type -> location -> t -> string -> result

(** [start t] is the result containing the new state after the start of the
    turn or illegal if the turn had already started, along with the integer
    corresponding to the value just "rolled". *)
val start : t -> result * int

(** [end_turn t] is the result containing the state after a player's turn has
    ended if the player's turn had not yet started and illegal otherwise. *)
val end_turn : t -> result

(** [build b l t] is the result containing the new state after a player has
    built a new building of type [b] at location [l] or illegal if that action
    was not possible*)
val build : build_type -> location -> t -> result

(** [scores t] is an association list mapping players' names to their scores.
*)
val scores : t -> (string * int) list

(** [resources t] returns the association list mapping resource names to the
    amount the current player has. *)
val resources : t -> (string * int) list

(** [resources_for_player player_name st] returns the association list
    mapping resource names to the
    amount for the player [player_name]. *)
val resources_for_player : t -> string -> (resource * int) list

(** [tile_list_from_gridnum_list tiles gridnums] is the tile list corresponding
    to [gridnums]*)
val tile_list_from_gridnum_list : tile list -> int list -> tile list

(** [grab_gridnums tiles] converts a tile list into a list of corresponding
    grid numbers *)
val grab_gridnums : tile list -> int list

(** [sort_by_gridnum tiles] is the sorted [tiles] list *)
val sort_by_gridnum : tile list -> tile list

(** [discard pname choices st] is the result after player [pname] discarded
    resources in [choices] *)
val discard : string -> (resource * int) list -> t -> result

(** [move_robber loc st] is the result after tthe robber is moved to gridnum
    [loc] *)
val move_robber : int -> t -> result

(** [get_players_to_steal st] is a list of player names that the current
    player can steal from *)
val get_players_to_steal : t -> string list

(** [steal pname st] is the result and resource tuple where the result is
    the result after the current player stole from player [pname], and the
    resource
    represents the resource the current player stole. *)
val steal : string -> t -> result * resource

(** [trade give receive target st player_name] is the result after the
    current player traded with [target], gave [give] and receivec [give] *)
val trade : (resource * int) list -> (resource * int) list -> string -> t
  -> string -> result

(** [accept give receive target st player_name] returns [Illegal] if
    [player_name] can't accept the trade or [Legal st'] where [st'] is the
    state after [player_name] has been charged the resources in [give] and
    credited the resources in [receive] and the opposite for [target]. *)
val accept : (resource * int) list -> (resource * int) list -> string -> t
  -> string -> result

(** [resources_from_placement pname loc] is the state after the player [pname]
    harvested resource by placing a settlement at location [loc] *)
val resources_from_placement : string -> location -> t -> t

(** [initial_roads loc settle st p] returns Legal of a state with the road
    built if [loc] is valid given [settle]. *)
val initial_roads : int list -> int list -> t -> string -> result

(** [get_turn_status st] is true if a turn is ongoing *)
val get_turn_status : t -> bool

(** [spot_taken loc buildings] is true if the location [loc] already contains
    a building *)
val spot_taken : location -> building list -> bool

(** [draw_card st] is the result and string tuple where the result is the
    result of the current player purchasing a card, and the string is the
    string
    representation of the card the player just bought *)
val draw_card : t -> result * string

(** [card_to_string card] is the string representation of the card [card]*)
val card_to_string : card -> string

(** [play_vp st cardname] is the result after the current player
    played the victory point card [cardname] *)
val play_vp : t -> string -> result

(** [play_yop st resources] is the result afeter the current player played the
    year of plenty card with the resources [resources] *)
val play_yop : t -> resource * resource -> result

(** [get_current_player_cards st] is the all the cards the current player
    owns*)
val get_current_player_cards : t -> card list

(** [building_at_location loc buildings] is the building at location [loc] *)
val building_at_location : location -> building list -> building

(** [play_monopoly st resource] is the result after the current player plays
    the monopoly card on [resource] *)
val play_monopoly : t -> resource -> result

(** [play_rb st locs] is the result after the current player plays the
    road building carc on locations [locs] *)
val play_rb : t -> int list * int list -> result

(** [play_knight st] is the result after the current player plays the knight
    card *)
val play_knight : t -> result

(** [should_harvest st tile] is true if the tile can produce resources *)
val should_harvest: t -> tile -> bool

(** [get_robber_tile st] is the grinum of where the robber is *)
val get_robber_tile : t -> int

(** [get_player_with_highest_score st exclude] is the current player with
    the highest score, excluding the player with name [exclude] *)
val get_player_with_highest_score : t -> string -> player

(** [has_city_or_settlement_by_tile st player gridnum] is true if the player
    [player] has a city or settlement by the tile [gridnum] *)
val has_city_or_settlement_by_tile : t -> string -> int -> bool

(** [path_from_settlement_to_vertex start player_name goal st range]*)
val path_from_settlement_to_vertex : int list -> string -> int list -> t ->
  int -> (int * int list list)

val shortest_path : int list ->
  int list ->
  int list list ->
  tile list -> building list -> int -> player -> int -> int * int list list

val grab_gridnums : tile list -> int list

val get_players : t -> player list
(** [can_draw st] indicates whether the current player is allowed to draw
    a card *)
val can_draw : t -> bool

(** [can_play_card st] indicates whether the current player is allowed to play
    a card*)
val can_play_card : t -> bool

(** [get_player_from_name name st] returns the player with name [name] in [st]
*)
val get_player_from_name : string -> t -> player

val get_roads : building list -> building list -> building list

(** [ai_rates st name] returns the association list of each resource with the
    rate at which [name] is allowed to trade it for other resources *)
val ai_rates : t -> string -> (resource * int) list
