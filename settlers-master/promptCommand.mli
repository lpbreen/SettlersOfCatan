(** The parser for the passive prompt commands to the players *)

(** The type [object_phrase] represents the object phrase that can be part of a
    player command.  Each element of the list represents a word of the object
    phrase, where a {i word} is defined as a consecutive sequence of non-space
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words
    in the original player command.

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list


(** The type [prompt_command] represents the player's desired action that is
    decomposed into the desired prompt_command as well as any arguments that
    prompt_command should take. *)
type prompt_command =
  | Discard of (State.resource * int) list
  | Trade of bool
  | Robber of int
  | Steal of string


(** Raised when an bad prompt_command is parsed *)
exception Illegal

(** [parse s] returns the prompt command, with the first word before a space
    forming the command and all following text parsed into
    arguments for that command.
*)
val parse : string -> prompt_command