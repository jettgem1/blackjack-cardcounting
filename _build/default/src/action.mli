type writing = string list

(** The type [writing] represents the filtered input of the user. Every
    element of the list represents a word inputed by the player. These
    words are used to determine which program to "start" or "end", and
    how much to bet. For example, if the user were to start the program,
    they could say "start tutorial". This would then start the tutorial.
    If a player was getting bored of the tutorial, they could enter "end
    tutorial". This would then take them back to the home screen. They
    could then proceed to call In order to end the entire program, one
    would need to say "end Blackjack" *)

(** The type [action] represents a user's input that is filtered into a
    action and further words*)
type action =
  | Bet of writing
  | Hit
  | Stay
  | Split
  | Double_down
  | Surrender
  | Insurance
  | Start of writing
  | End of writing

exception Empty
(** Raised when an empty action is read. *)

exception Malformed
(** Raised when a malformed action is read. *)

val read : bool -> string array -> string -> action
(** [read str] reads a user's message into [action]. The first word of
    [str] becomes the "command." The rest of the words, if any, become
    the writing.*)
