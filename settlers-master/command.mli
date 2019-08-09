(** Parser for the player's active commands during their turns. *)

(** The type [loc_phrase] represents the location specified by the user in a
    [Build] command. *)
type loc_phrase = int list

(** The type [object_phrase] represents the object phrase that can be part of a
    player command.  Each element of the list represents a word of the object
    phrase, where a {i word} is defined as a consecutive sequence of non-space
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words
    in the original player command.

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list


(** The type [command] represents the player's desired action that is
    decomposed into the desired command as well as any arguments that
    command should take*)
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

(** Raised when an empty command is parsed *)
exception Empty

(** Raised when a malformed command is parsed *)
exception Malformed

(** [parse s] returns the command, with the first word before a space forming
    the command and all following text parsed into arguments for that command.
*)
val parse : string -> command
