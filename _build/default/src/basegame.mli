(**the basegame contains all the functions that takes the json file and
   returns data the game can make use of*)

exception EmptyDeck

type card
(**the type of a card*)

type deck
(**the type of a deck*)

type current_hand
(**the type of a hand*)

type game
(** the type of a game*)

type result =
  | Win
  | Lose
  | Draw

type condition =
  | Under
  | Win
  | Bust  (** Whether or not the player is under 21, at 21, or over 21*)

type cnt_mthd =
  | HiLo
  | NoMthd

val deck_of_json : Yojson.Basic.t -> deck
(**turns a json file into a deck*)

val top_card : deck -> card
(** the top card of the deck or if empty, raises exception EmptyDeck*)

val all_cards : deck -> string list
(** all cards in the current deck (their names)*)

val shuffle_deck : deck -> deck
(**takes a deck and shuffles it, but keeps the same amount of cards*)

val deal : game -> game
(** returns game with two cards dealt to the dealer and player*)

val hit : game -> string -> game
(** returns game with a card dealt to a person*)

val create_game : int -> int -> deck -> game
(** creates a new game from a blank deck*)

val begin_game : game -> game
(**begins a new game and deals the cards out*)

val check_condition : current_hand -> condition
(**[check_condition current_hand] is Under if the current hand is under
   21, Win if the current hand is 21, and a Bust if the current hand is
   greater than 21*)

val get_player_hand : game -> current_hand
(**[get_player_hand game] is the current hand of the player*)

val get_dealer_hand : game -> current_hand
(**[get_player_hand game] is the current hand of the dealer*)

val get_cards : current_hand -> card list

val get_card_value : card -> int

val get_card_name : card -> string

val get_card_suit : card -> string

val print_hand : current_hand -> unit

val get_cards_left : game -> int

val get_current_hand_card_names : current_hand -> string list

val get_hand_value : current_hand -> int

val add_to_balance : game -> int -> game

val set_balance : game -> int -> game

val is_blackjack : card list -> bool

val dealer_turn :
  game -> bool -> bool -> int -> bool -> cnt_mthd -> result

val check_tutorial : game -> bool

val get_balance : current_hand -> int

val new_game : game -> game

val check_insurance : game -> bool

val check_insurance_tester : card list -> bool

val check_double_down : game -> bool

val check_double_down_tester : string list -> bool

val check_surrender_tester : string list -> bool

val check_split : game -> bool

val check_surrender : game -> bool

val possible_actions : game -> string list

val action_string : game -> string

val find_ranks_left : game -> string -> int

val print_both_hands : game -> bool -> bool -> cnt_mthd -> unit

val dealer_shows : game -> string

val make_split : game -> game

val is_split : game -> bool

val flip_hands : game -> game

val set_deck : deck -> int -> deck