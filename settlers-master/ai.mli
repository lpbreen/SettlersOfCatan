(** The Command generator for the AI players. *)

(** [choose_color chosen] is the color chosen by the AI that is not in the
    list [chosen] *)
val choose_color : State.player list -> State.color

(** [choose_name chosen] is the name chosen by the AI that is not in the list
    [chosen] *)
val choose_name : State.player list -> string

(** [command st player] is the command the ai player [player] inputs on his
    turn. *)
val command : State.t -> State.player -> string

(** [discard st player amount] is the prompt command from the AI player
    [player], which specifies which resources it wishes to discard *)
val discard : State.t -> State.player -> int -> string

(** [move_robber st] is the prompt command from the AI player that
    specifies which tile it wants to move the robber to *)
val move_robber : State.t -> string


(** [steal st players_to_steal] is the prompt comand from the AI player that
    specifies who to steal from, given the list of player names
    [players_to_steal] that it could steal from. *)
val steal : State.t -> string list -> string

(** [trade st give receive player] is the prompt comand from the AI player
    [player] that specifies whether or not to accept a trade. The player will
    accept the trade if the expectation of getting the lost resources is 
    greater
    than the expectation of getting the received resources *)
val trade : State.t -> (State.resource * int) list 
  -> (State.resource * int) list -> State.player -> string

(** [place_initial_settlement st name] is the prompt command from the AI player
    [name] for an initial settlement placement based on the "best" location
    available in [st]. *)
val place_initial_settlement : State.t -> string -> string

(** [place_initial_road st name settlement_loc] is the prompt command from the
    AI player [name] for a starting road next to the settlement it just placed
    at [settlement_loc] based on information from [st]. *)
val place_initial_road : State.t -> string -> int list -> string

(** [ai_draw_card state cards]  draws a card from the bank based on if the at 
    least enough resources available to buy cards from the supply stacks . *)
val ai_draw_card: State.t -> string * bool

(** [ai_play_card state cards] plays a card and executes the specific
    function to play the card. *)
val ai_play_card: State.t -> string * bool


