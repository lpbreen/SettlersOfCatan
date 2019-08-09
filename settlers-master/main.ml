open ANSITerminal
open State
open PromptCommand

(** [help_message] is the help message displayed to the player when they enter
    the command "help" *)
let help_message = "Here are a list of Possible Commands: \n" ^
                   "start: start the game \n" ^
                   "build [building] [location(in the format
                   of 'a b c')] \n" ^
                   "trade [player name] offer [resource1] [amount1] \
                    [resource2] [amount2] ... receive [resource1] [amount1] \
                    [resource2] [amount2]" ^
                   "draw: draw a card.\n" ^
                   "knight: use a knight card, if you have one.\n" ^
                   "victorypoint [name]: use a victory point card, if you \
                    have one.\n" ^
                   "yearofplenty [resource1] [resource2]: use a year of \
                    plenty card, if you have one.\n" ^
                   "monopoly [resource]: use a monopoly card, \
                    if you have one. \n"^
                   "roadbuilding location [tile1] [tile2] location \
                    [tile3] [tile4]: use a road building card, \
                    if you have one. \n" ^
                   "costs: check the costs of buildings\n" ^
                   "resources: check the players and their resources\n" ^
                   "scores: check the players and their scores\n" ^
                   "end: end a turn\n" ^
                   "help: get a list of possible commands\n" ^
                   "exit: exit the game : (\n"

(** [terminal_color_from_state_color color] is the terminal color corresponding
    to the state color [color]*)
let terminal_color_from_state_color color =
  match color with
  | Red -> red
  | Green -> green
  | Yellow -> yellow
  | Blue -> blue
  | Magenta -> magenta
  | Cyan -> cyan
  | Black -> black
  | White -> white

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

(** [print_r_i_lst lst] prints the resource and int tuple list *)
let rec print_r_i_lst lst = match lst with
  | [] -> ()
  | (resource, amount)::t ->
    print_string [default] ("Resource: " ^
                            string_of_resource resource ^ ", Amount: "
                            ^ string_of_int amount ^ "\n");
    print_r_i_lst t

(** [print_board st roll] prints the board corresponding to state [st] and
    displays the current dice roll [roll] *)
let print_board st roll =
  ANSITerminal.erase ANSITerminal.Screen;
  let width,height = State.get_dimensions st in
  Board.print_board width height st roll

(** [first_non_empty str_lst] is the first non-empty string in [str_lst]. *)
let rec first_non_empty str_lst =
  match str_lst with
  | [] -> "Foo"
  | h::t -> if h <> "" then h else first_non_empty t

(** [prompt_expansion()] is the type of expansion the users entered *)
let rec prompt_expansion () =
  print_string [Foreground Default]
    "Please enter the expansion you desire to play \n \
     You can choose from: \n \
     - Basic \n> ";
  match read_line() with
  | response -> match String.lowercase_ascii response with
    | "basic" -> State.Basic
    | _ -> print_string [Foreground Default]
             "Unrecognized expansion, Try again. ";
      prompt_expansion ()

(** [player_name_taken players name] is true if the list [players] already
    contains the player name [name] *)
let rec player_name_taken players name = match players with
  | [] -> false
  | h::t -> if h.name = name then true
    else player_name_taken t name

(** [player_name_taken players color] is true if the list [players] already
    contains the color [color] *)
let rec player_color_taken players color = match players with
  | [] -> false
  | h::t -> if h.color = color then true
    else player_color_taken t color

(** [prompt_player_num is_ai] is the number of players entered by the players
*)
let rec prompt_player_num is_ai lower upper =
  if is_ai then (
    print_string [Foreground Default]
      ("Please enter the number of AI players within the range "
       ^ string_of_int lower ^ " to " ^ string_of_int upper ^ "\n> ");
  )
  else (
    print_string [Foreground Default]
      ("Please enter the number of human players within the range "
       ^ string_of_int lower ^ " to " ^ string_of_int upper ^ "\n> "));
  match read_line() with
  | response -> let player_count = try int_of_string response
                  with Failure _ -> print_string [Foreground Default]
                                      "Invalid number, please try again.";
                    prompt_player_num is_ai lower upper in
    if player_count >= lower && player_count <= upper then
      player_count
    else(
      print_string [Foreground Default]
        "Invalid number, please try again.";
      prompt_player_num  is_ai lower upper)

(** [prompt_player_info index count acc] is a list of players
    whose properties are specified by the user *)
let rec prompt_player_info
    (current_player: int)
    (player_count: int)
    (acc : player list) : player list =
  if current_player = player_count then acc
  else
    (
      print_string [Foreground Default]
        ("Entering Info for player " ^
         string_of_int current_player ^ ": ");
      print_string [Foreground Default]
        "Please enter the color, choose \
         from red, green, yellow, magenta, blue, cyan \n> ";
      let rec color() =
        let chosen_color = match read_line() with
          | response -> match String.lowercase_ascii response with
            | "red" -> Red
            | "green" -> Green
            | "yellow" -> Yellow
            | "magenta" -> Magenta
            | "blue" -> Blue
            | "cyan" -> Cyan
            | _ -> (print_string [red]
                      "Invalid color, please try again. \n> ";
                    color ())
        in
        if player_color_taken acc chosen_color then (
          print_string [red]
            "This color is already taken by \
             another player. Please try again. \n";
          color()
        )
        else
          chosen_color
      in
      let chosen_color = color() in

      print_string [Foreground Default]
        "Please Enter the name of the player \n> ";
      let name:string = match read_line() with
        | response -> String.lowercase_ascii
                        (first_non_empty (String.split_on_char ' ' response))
      in
      if player_name_taken acc name then
        (
          print_string [red] "This name is already taken by \
                              another player. Please try again. \n> ";
          prompt_player_info current_player player_count acc
        )
      else(
        let new_player : player = {name = name; score = 0;
                                   color = chosen_color;
                                   resources = [(Brick, 0); (Sheep,0);
                                                (Grain,0);
                                                (Rock,0); (Wood,0)];
                                   cards=[]; is_ai = false }
        in
        prompt_player_info (current_player + 1) player_count
          (new_player::acc)
      )
    )

let rec prompt_ai_info current_player count acc =
  if current_player = count then acc else (
    let name = Ai.choose_name acc in
    let color = Ai.choose_color acc in
    let new_player : player = {name = name; score = 0;
                               color = color;
                               resources = [(Brick, 0); (Sheep,0);
                                            (Grain,0);
                                            (Rock,0); (Wood,0)];
                               cards=[]; is_ai = true }
    in
    prompt_ai_info (current_player + 1) count (new_player::acc)
  )

(** [prompt_players()] is a list of players the users entered *)
let prompt_players () =
  let human_players_count = prompt_player_num false 0 5 in
  let ai_players_count = prompt_player_num true 0 (5 - human_players_count) in
  let human_players = prompt_player_info 0 human_players_count [] in
  prompt_ai_info 0 ai_players_count human_players

(** [gen_unrepeated r min max] is a psudo-random number generated within the
    bound of [min] and [max], and not in the list [r]*)
let rec gen_unrepeated repeats min max =
  let generator = QCheck.Gen.int_range min max in
  let num = QCheck.Gen.generate1 generator in
  print_int max;
  flush_all();
  if List.mem num repeats then
    gen_unrepeated repeats min max
  else
    num

(** [gen_player_order players acc used min] is a psudo-randomly generated
    accoc list of order mapped to players, with order starting from [min]*)
let rec gen_player_order players acc used min count=
  match players with
  | [] -> acc
  | h::t -> let order = gen_unrepeated used min (min + count - 1) in
    gen_player_order t ((order, h)::acc) (order::used) min count

(** [print_player_order player_order] prints the order of the players
    as they are in. *)
let print_player_order player_order =
  let player_order_sorted = List.sort Pervasives.compare player_order in
  let rec print_player_order_help player_order_sorted =
    match player_order_sorted with
    | [] -> ()
    | (order, player)::t ->
      let color = terminal_color_from_state_color player.color in
      print_string [color] ("Player " ^ string_of_int order ^ " is " ^
                            player.name);
      (if player.is_ai then
         (print_string [color] " (A.I. Player) \n";)
       else
         print_string [color] "\n";);

      print_player_order_help t
  in
  print_player_order_help player_order_sorted

(** [prompt_init_settlements_and_roads po st] is state of the game after the
    sorted players [po] have all entered an initial settlement and a
    road by the
    initial settlement. *)
let rec prompt_init_settlements_and_roads
    sorted_player_order
    acc
    should_get_points
  : State.t =
  let rec build_road_help st player next_player_order order settlement_loc =
    print_board st None;
    print_string [terminal_color_from_state_color player.color]
      ("Player " ^ string_of_int order ^ " " ^ player.name ^
       " please build an initial road by the settlement you just created.\n ");
    print_string [default]
      "Run the \"build road a b \" command. \n> ";
    let input = if player.is_ai then
        let ai_input = Ai.place_initial_road st player.name settlement_loc in
        print_string [default] (ai_input ^ "\n");
        ai_input
      else (
        match read_line() with | response ->
          String.lowercase_ascii response ) in
    match Command.parse input with
    | Command.Build (Road, location) ->
      (
        match State.initial_roads location settlement_loc st player.name with
        | exception Failure str -> print_string [red]
                                     ("Failed with message: " ^
                                      str ^ " Try again. \n");
          build_road_help st player next_player_order order settlement_loc
        | Legal st' -> prompt_init_settlements_and_roads next_player_order
                         st' should_get_points
        | _ -> print_string [red] "Failed. Please make sure you are \
                                   building a road that is by the \
                                   settlement. Try again. \n";
          build_road_help st player next_player_order order settlement_loc
      )
    | _ ->  print_string [red] "Failed. Please make sure you are \
                                building a road that is by the \
                                settlement. Try again. \n";
      build_road_help st player next_player_order order settlement_loc
    | exception Command.Empty -> print_string [red] "Failed. No empty \
                                                     commands. Try \
                                                     again. \n";
      build_road_help st player next_player_order order settlement_loc
    | exception Command.Malformed -> print_string [red] "Failed. \
                                                         Unrecognized \
                                                         command. Try \
                                                         again. \n";
      build_road_help st player next_player_order order settlement_loc
  in
  let rec build_settlement_help st player order =
    print_board st None;
    print_string [terminal_color_from_state_color player.color]
      ("Player " ^ string_of_int order ^ " " ^ player.name ^
       " please build an initial settlement.\n ");
    print_string [default]
      "Run the \"build settlement a b c \" command. \n> ";
    let input = if player.is_ai then
        let ai_input = Ai.place_initial_settlement st player.name in
        print_string [default] (ai_input ^ "\n");
        ai_input
      else (
        match read_line() with | response ->
          String.lowercase_ascii response ) in
    match input with
    | response -> match Command.parse (String.lowercase_ascii response) with
      | Command.Build (Settlement, location) ->
        (
          let loc = (State.tile_list_from_gridnum_list
                       (State.get_tiles st) location) in
          match State.init_placements Settlement loc st player.name with
          | exception Failure str -> print_string [red] ("Failed with \
                                                          message: " ^
                                                         str ^
                                                         " Try again. \n");
            build_settlement_help st player order
          | Legal st' -> if should_get_points then
              (State.resources_from_placement player.name loc st', location)
            else (st', location)
          | _ -> print_string [red] "Failed. Please make sure you \
                                     are building a settlement that \
                                     is not adjacent to another \
                                     settlement. Try again.\n";
            build_settlement_help st player order
        )
      | _ ->  print_string [red] "Failed. Please make sure you are building \
                                  a settlement that is not adjacent to \
                                  another settlement. Try again.\n";
        build_settlement_help st player order
      | exception Command.Empty -> print_string [red] "Failed. No empty \
                                                       commands. \
                                                       Try again. \n";
        build_settlement_help st player order
      | exception Command.Malformed -> print_string [red] "Failed. \
                                                           Unrecognized \
                                                           command. Try \
                                                           again. \n";
        build_settlement_help st player order
  in
  match sorted_player_order with
  | [] -> acc
  | (order, player)::tail ->
    try (let settlement_built_state_loc_pair =
           build_settlement_help acc player order in
         build_road_help (fst settlement_built_state_loc_pair)
           player tail order (snd settlement_built_state_loc_pair)) with
    | failure -> print_string [red] "Failed. Try again. \n";
      prompt_init_settlements_and_roads sorted_player_order acc
        should_get_points


(** [sum_r_i_list lst acc] is the sum of the resource and int tuple list
    [lst] *)
let rec sum_r_i_list lst acc =
  match lst with
  | [] -> acc
  | (_, intVal)::t -> sum_r_i_list t (acc + intVal)

(** [discard_help lst acc] is the state after players selected resources to
    discard. [lst] is the resource and int tuple list that specifies how many
    resources each player has to discard *)
let rec discard_help lst acc = match lst with
  | [] -> acc
  | (player, count)::t ->
    print_string [terminal_color_from_state_color player.color]
      ("Player " ^ player.name ^ " please select " ^ string_of_int count
       ^ " resources to drop using the following command: \n");
    print_string [default] "discard [resource1] [amount1] [resource2] \
                            [amount2] ...\n";
    print_string [default] "Here is the list of resources you have: \n";
    print_r_i_lst player.resources;
    print_string [default] "> ";
    let input = if player.is_ai then
        let ai_input = Ai.discard acc player count in
        print_string [default] (ai_input ^ "\n");
        ai_input
      else (
        match read_line() with | response ->
          String.lowercase_ascii response ) in
    match PromptCommand.parse input with
    | exception Failure _ -> print_string [red] "Invalid discard command. \
                                                 Try again.\n";
      discard_help lst acc
    | exception Illegal -> print_string [red] "Invalid discard command. \
                                               Try again.\n";
      discard_help lst acc
    | Discard r_i_lst -> if (sum_r_i_list r_i_lst 0) = count then
        (
          match State.discard player.name r_i_lst acc with
          | Legal st' -> discard_help t st'
          | _ ->
            print_string [red] "Discard Failed in state. Try again.\n";
            discard_help lst acc
        )
      else
        (
          print_string [red] ("Please enter a \
                               total of " ^ string_of_int count
                              ^ " resources to \
                                 discard. Try again.\n");
          discard_help lst acc
        )
    | _ -> print_string [red] "Invalid discard command. \
                               Try again.\n";
      discard_help lst acc

(** [robber_help st] is the state after the player selected where to move
    the robber*)
let rec robber_help st =
  print_string [terminal_color_from_state_color
                  (State.get_current_player st).color]
    ("Now the current player " ^
     (State.get_current_player st).name ^
     " please enter the gridnum you want to move the robber \
      to. Use the command \"robber a\"\n> ");
  let input = if (State.get_current_player st).is_ai then
      let ai_input = Ai.move_robber st in
      print_string [default] (ai_input ^ "\n");
      ai_input
    else ( match read_line() with | response -> response ) in
  match PromptCommand.parse input with
  | exception Failure _ -> print_string [red] "Invalid  command. \
                                               Try again.\n";
    robber_help st
  | exception Illegal -> print_string [red] "Invalid  command. \
                                             Try again.\n";
    robber_help st
  | Robber gridnum ->
    (
      match State.move_robber gridnum st with
      | Legal st' -> st'
      | _ -> print_string [ANSITerminal.red]
               "Moving robber failed in state. Try again. \n";
        robber_help st
    )
  | _ -> print_string [ANSITerminal.red]
           "Invalid robber command. Try again. \n";
    robber_help st

(** [steal_help st] is the state after the player chooses which player to steal
    from *)
let rec steal_help st=
  let players_to_steal = State.get_players_to_steal st in
  if players_to_steal = [] then
    st
  else (
    print_string [terminal_color_from_state_color
                    (State.get_current_player st).color]
      ("Now the current player " ^
       (State.get_current_player st).name ^
       " please enter the player you want to steal from\
        . Use the command \"steal player_name\".\n The players you \
        can steal from are: \n");
    let rec print_string_in_list lst = match lst with
      | [] -> ()
      | h::t -> print_string [default] (h ^ "\n");
        print_string_in_list t
    in
    print_string_in_list players_to_steal;
    print_string [default] "> ";
    let input = if (State.get_current_player st).is_ai then
        let ai_input = Ai.steal st players_to_steal in
        print_string [default] (ai_input ^ "\n");
        ai_input
      else ( match read_line() with | response -> response ) in
    match PromptCommand.parse input with
    | exception Failure _ -> print_string [red] "Invalid steal command. \
                                                 Try again.\n";
      steal_help st
    | exception Illegal -> print_string [red] "Invalid steal command. \
                                               Try again.\n";
      steal_help st
    | Steal player_name ->
      (
        match State.steal player_name st with
        | (Legal st'), resource ->
          print_string [default] ("You stole a piece of " ^
                                  (string_of_resource resource) ^
                                  " from player " ^
                                  player_name ^ ".\n");
          st'
        | _ -> print_string [ANSITerminal.red]
                 "Stealing Failed. Make sure you have a right player name. \
                  Try again \n";
          steal_help st
      )
    | _ -> print_string [ANSITerminal.red]
             "Invalid steal command. Try again. \n";
      steal_help st
  )

(** [knight_or_roll_of_seven_help st lst] processes the condition where a
    knight card is played or a 7 is drawn. [lst] is the player and int tuple
    list that contains the amount of resources each player is
    expected to discard *)
let knight_or_roll_of_seven_help st lst =
  let state_after_discard = discard_help lst st in
  let state_after_robber = robber_help state_after_discard in
  steal_help state_after_robber

let rec prompt_command st (diceroll: int option) =
  print_board st diceroll;
  print_string [Foreground Default]
    "\n--------------------------------------------------------------\
     \nWelcome to the 3110 Settlers of Catan Game engine.\
     Use \"help\" to see all available Commands.\n"
  ;
  if State.get_turn_status st then
    let player = State.get_current_player st in
    print_string [terminal_color_from_state_color player.color]
      ("A turn is currently ongoing for player " ^
       player.name ^ ".\n");
    print_string [default] "Please enter your command: \n> "
  else (
    print_string [default] "The last player has finished their turn. Please \
                            enter command \"start\" to roll dices and start\
                            your turn.\n";
    let next_player = State.get_current_player st in
    print_string [terminal_color_from_state_color next_player.color]
      ("The next\
        player is: " ^ next_player.name ^ ".\n");
    print_string [default] "Please enter the \"start\" command: \n> "
  );
  let input = if (State.get_current_player st).is_ai then
      let ai_input = Ai.command st (State.get_current_player st) in
      print_string [default] (ai_input ^ "\n");
      ai_input
    else ( match read_line() with | response -> response ) in
  match input with
  | response -> match Command.parse response with
    exception Command.Empty ->
    ANSITerminal.(print_string [red]
                    "Empty command. Use command \"help\" to see \
                     all available Commands. \n");
    prompt_command st diceroll
    | exception Command.Malformed ->
      ANSITerminal.(print_string [red]
                      "Unrecognized command. Use command \"help\" to see \
                       all available Commands. \n");
      prompt_command st diceroll
    | Build (b, l) -> (match State.build b (State.tile_list_from_gridnum_list
                                              (State.get_tiles st) l) st with
                      | exception Failure str ->
                        print_string [red]
                          ("Failed with message: " ^ str ^ " Try again. \n");
                        prompt_command st diceroll
                      | Illegal -> print_string [red]
                                     "Unable to fulfill build command";
                        prompt_command st None
                      | Legal st' -> prompt_command st' diceroll
                      | Winner player ->
                        print_string [ANSITerminal.red]
                          ("We have a Winner! Game is over. \
                            Congratulations to player " ^
                           player.name)
                      | Prompt _ -> print_string [ANSITerminal.red]
                                      "Build shouldn't ever return a prompt" )
    | Start ->
      (match State.start st with
       | Illegal, _ -> print_string [red]
                         "Encountered Illegal while trying to \
                          perform a start command";
         prompt_command st diceroll
       | (Legal st'), d -> print_string [Foreground Default]
                             ("The dice roll was " ^ (string_of_int d));
         prompt_command st' (Some d)
       | Winner player, _ -> print_string [ANSITerminal.red]
                               ("We have a Winner! Game is over. \
                                 Congratulations to player " ^
                                player.name)
       | Prompt prompt, roll ->
         match prompt with
         | Discard (lst, discarding_state) ->
           print_board discarding_state (Some roll);
           print_string [default] ("Looks like we've have a dice roll of "
                                   ^ string_of_int roll ^
                                   ". \n");
           prompt_command
             (knight_or_roll_of_seven_help discarding_state lst) (Some roll)
         | _ -> print_string [red]
                  "Start shouldn't ever return a prompt that is not discard";
      )
    | End -> (match State.end_turn st with
        | Illegal -> print_string [red]
                       "Encountered Illegal while trying to \
                        perform an end command";
          prompt_command st diceroll
        | Legal st'->  prompt_command st' None
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               Congratulations to player " ^
                              player.name)
        | Prompt _ -> (print_string [ANSITerminal.red]
                         "end should't ever give back a prompt" );
          prompt_command st diceroll)
    | Help -> print_string [Foreground Default] help_message;
      prompt_command st diceroll
    | Exit -> exit 0
    | Resources -> List.iter (fun (a,b) ->
        print_string [Foreground Default]
          (a ^ ": " ^ (string_of_int b) ^ " \n"))
        (State.resources st);
      prompt_command st diceroll
    | Scores -> List.iter (fun (a,b) -> print_string [Foreground Default]
                              (a ^ ": " ^ (string_of_int b) ^ " \n"))
                  (State.scores st);
      prompt_command st diceroll
    | Trade (target, offer_list, receive_list) ->
      (
        match State.trade offer_list receive_list target st
                (State.get_current_player st).name with
        | exception Failure str ->
          print_string [red]
            ("Failed with message: " ^ str ^ " Try again. \n");
          prompt_command st diceroll
        | Legal st' -> print_string [default] "trade successful! \n";
          prompt_command st' diceroll
        | Prompt prompt -> (
            match prompt with
            | Trade (target, offer_lst, receive_lst, st') ->
              print_string [default]
                ("Player " ^ target.name
                 ^ ", player "
                 ^ (State.get_current_player st').name
                 ^ " wishes to start a trade with you. \
                    Here are their offer: \n");
              print_r_i_lst offer_lst;
              print_string [default] "Here are what they expect from you: \n";
              print_r_i_lst receive_lst;
              print_string [default] "Here are the resources you have: \n";
              print_r_i_lst (State.resources_for_player st' target.name);
              print_string [default] "Use the \
                                      command \"trade accept\" \
                                      or \"trade reject\" to \
                                      either accept or reject \
                                      the trade offer. \n> ";
              let rec trade_help () =
                let input = if target.is_ai then
                    let ai_input = Ai.trade st' receive_list offer_list target
                    in
                    print_string [default] (ai_input ^ "\n");
                    ai_input
                  else (
                    match read_line() with | response ->
                      String.lowercase_ascii response ) in
                (match PromptCommand.parse input with
                 | Trade true ->
                   (
                     match State.accept
                             receive_lst
                             offer_lst
                             (State.get_current_player st').name st'
                             target.name with
                     | Legal st'' ->
                       let current_player = State.get_current_player st' in
                       print_string
                         [terminal_color_from_state_color
                            current_player.color]
                         ("You have traded successfully! \
                           he current player "
                          ^ current_player.name
                          ^ " please continue your turn. \n");
                       prompt_command st'' diceroll
                     | _ -> print_string [red] "the other player accepted \
                                                your offer, but doesn't \
                                                have enough to pay you.\n";
                       prompt_command st' diceroll
                   )
                 | Trade false ->
                   let current_player =
                     State.get_current_player st' in
                   print_string
                     [terminal_color_from_state_color
                        current_player.color]
                     ("The other player has rejected your \
                       trade proposal. The current player "
                      ^ current_player.name
                      ^ " please continue your turn. \n");
                   prompt_command st' diceroll
                 | _ -> print_string [red] "Invalid Command. Try again.\n> ";
                   trade_help()
                 | exception Illegal -> print_string [red]
                                          "Invalid Command. Try again. \n";
                   trade_help()
                )
              in
              trade_help()
            | _ -> print_string [red] "Hmm.. Trade shouldn't \
                                       return a prompt that is \
                                       not a trade prompt";
              prompt_command st diceroll
          )
        | _ -> print_string [red]
                 "trade failed. Make sure you have the right \
                  player names, and they have more resources than \
                  what you requested. \
                  \nIf you are trading with the bank, make \
                  sure you have entered the \
                  right amount.";
          prompt_command st diceroll
      )
    | Draw -> (
        match State.draw_card st with
        | Legal st', card_name ->
          print_string [default] ("The card you draw was: "
                                  ^ card_name ^ "\n");
          prompt_command st' diceroll
        | _ -> print_string [red] "You don't have enough \
                                   resources to draw a card. \n";
          prompt_command st diceroll
      )
    | VictoryPointCard card_name ->
      (
        match State.play_vp st card_name with
        | Legal st' -> print_string [default] "success!\n";
          prompt_command st' diceroll
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               Congratulations to player " ^
                              player.name)
        | _ -> print_string [red] "Failed. Check that you \
                                   have a victory point card.\n";
          prompt_command st diceroll
      )
    | YearOfPlentyCard resource_tuple -> (
        match State.play_yop st resource_tuple with
        | Legal st' -> print_string [default] "success!\n";
          prompt_command st' diceroll
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               CongraTulations to player " ^
                              player.name)
        | _ -> print_string [red] "Failed. Check that you \
                                   have a year of plenty card.\n";
          prompt_command st diceroll
      )
    | MonopolyCard resource -> (
        match State.play_monopoly st resource with
        | Legal st' -> print_string [default] "success!\n";
          prompt_command st' diceroll
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               Congratulations to player " ^
                              player.name)
        | _ -> print_string [red] "Failed. Check that you \
                                   have a monopoly card.\n";
          prompt_command st diceroll
      )
    | RoadBuildingCard loc_tuple -> (
        match State.play_rb st loc_tuple with
        | Legal st' -> print_string [default] "success!\n";
          prompt_command st' diceroll
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               Congratulations to player " ^
                              player.name)
        | _ -> print_string [red] "Failed. Check that you have \
                                   a road building card.\n";
          prompt_command st diceroll
      )
    | KnightCard -> (
        match State.play_knight st with
        | Prompt prompt -> (
            match prompt with
            | Discard (lst, discarding_state) ->
              print_string [default] ("Knight Card initiated. \n");
              prompt_command
                (knight_or_roll_of_seven_help discarding_state lst) diceroll
            | _ -> print_string [red]
                     "Knight Card shouldn't ever return a prompt \
                      that is not discard";
          )
        | Winner player -> print_string [ANSITerminal.red]
                             ("We have a Winner! Game is over. \
                               Congratulations to player " ^
                              player.name)
        | _ -> print_string [red] "Failed. Check that you have a \
                                   play knight card.\n";
          prompt_command st diceroll
      )
    | _ -> prompt_command st diceroll


let play_game () =
  let expansion = prompt_expansion () in
  let players = prompt_players () in
  let player_order = gen_player_order players [] [] 0 (List.length players) in
  print_string [default] "The players' orders are generated randomly. \
                          They are: \n";
  print_player_order player_order;
  let init_state = State.init expansion players player_order in
  print_string [default] "Now enter the first round of initial settlements \
                          and players in order of the players.\n";
  let sorted_player_order = List.sort Pervasives.compare player_order in
  let first_round_init = prompt_init_settlements_and_roads sorted_player_order
      init_state false in
  print_string [default] "Now enter the second round of initial \
                          settlements and players in reverse order.\n";
  let second_round_init = prompt_init_settlements_and_roads
      (List.rev sorted_player_order) first_round_init true in
  prompt_command second_round_init None


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  play_game ()

(* Execute the game engine. *)
let () = main ()
