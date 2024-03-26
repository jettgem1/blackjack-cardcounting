open Game

module type BasegameSig = sig
  type card
  (**the type of a card*)

  type deck
  (**the type of a deck*)

  type current_hand
  (**the type of a hand*)

  type game
  (** the type of a game*)

  val deck_of_json : Yojson.Basic.t -> deck
  (**turns a json file into a deck*)

  val all_cards : deck -> string list
  (** all cards in the current deck*)

  val shuffle_deck : deck -> deck
  (**takes a deck and shuffles it, but keeps the same amount of cards*)

  val deal : game -> game
  (** returns game with two cards dealt to the dealer and player*)

  val begin_game : game -> game
  (**begins a new game and deals the cards out*)

  val create_game : int -> int -> deck -> game
  (** creates a new game from a blank deck*)
end

module BasegameCheck : BasegameSig = Basegame

module type ActionSig = sig
  type writing = string list

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

  exception Malformed

  val read : bool -> string array -> string -> action
end

module ActionCheck : ActionSig = Action
