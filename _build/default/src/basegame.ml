open Yojson.Basic.Util
(** General functions written for the blackjack program *)

let _ = Random.self_init ()

let start_amt = 1000

type person =
  | Player
  | Dealer

type card = {
  name : string;
  suit : string;
  rank : string;
  value : int;
}
(**a normal card type *)

type deck = {
  cards : card list;
  cards_left : int;
  memory_cards : card list;
}
(**the type of a deck*)

type current_hand = {
  hand : card list;
  value : int;
  person_type : person;
  balance : int;
}
(**type of a hand *)

type game = {
  card_deck : deck;
  players_hand : current_hand;
  split_hand : current_hand;
  dealers_hand : current_hand;
  hi_lo_cnt : int;
  is_tutorial : bool;
  deck_count : int;
}
(**type of the game*)

type condition =
  | Under
  | Win
  | Bust

type result =
  | Win
  | Lose
  | Draw

type cnt_mthd =
  | HiLo
  | NoMthd

exception EmptyDeck

(** *)
let cards_of_json json =
  {
    name = json |> member "name" |> to_string;
    suit = json |> member "suit" |> to_string;
    rank = json |> member "rank" |> to_string;
    value = json |> member "value" |> to_int;
  }

(*[deck_of_json json] creates a deck from a parsed json file*)
let deck_of_json json =
  let crds =
    json |> member "cards" |> to_list |> List.map cards_of_json
  in
  { cards = crds; cards_left = List.length crds; memory_cards = crds }

(** random card in the current deck*)
let random_card (lst : card list) num = List.nth lst (Random.int num)

(** the top card of the deck*)
let top_card dck =
  match dck.cards with [] -> raise EmptyDeck | h :: t -> h

(**sorts through a card list and returns a list of all the card names*)
let rec helper_name_list crds (acc : string list) =
  match crds with
  | [] -> acc
  | h :: t -> begin
      match h with { name = nm; _ } -> helper_name_list t (nm :: acc)
    end

(** names (strings) of all the cards in the current deck*)
let all_cards dck = helper_name_list dck.cards []

(*[helper_remove_card lst acc card] removes a specific card from lst and
  returns the new lst with that card removed*)
let rec helper_remove_card lst acc card =
  match lst with
  | [] -> List.rev acc
  | h :: t ->
      if h = card then acc @ List.rev t
      else helper_remove_card t (h :: acc) card

(** the card list without the top card of the deck*)
let deal_one dck =
  {
    cards = List.tl dck.cards;
    cards_left = dck.cards_left - 1;
    memory_cards = dck.memory_cards;
  }

(**take a card list and return a new one (with the same cards) shuffled *)
let rec helper_shuffle (lst : card list) (acc : card list) num =
  match lst with
  | [] -> acc
  | h :: _ ->
      let temp = random_card lst num in
      helper_shuffle
        (helper_remove_card lst [] temp)
        (temp :: acc) (num - 1)

(**takes a deck and shuffles it, but keeps the same amount of cards*)
let shuffle_deck dck =
  let num = List.length dck.cards in
  {
    cards = helper_shuffle dck.cards [] num;
    cards_left = num;
    memory_cards = dck.memory_cards;
  }

(**This takes a deckc and an int and returns a the deck multiplied by
   cnt*)
let rec grow_deck crds cnt acc =
  if cnt > 0 then grow_deck crds (cnt - 1) (acc @ crds) else acc

(**This takes a deckc and an int and returns a the deck multiplied by
   the*)
let set_deck dck cnt =
  let expanded_deck = grow_deck dck.cards cnt [] in
  {
    cards = expanded_deck;
    cards_left = List.length expanded_deck;
    memory_cards = expanded_deck;
  }

(*[find aces lst] is the number of ace cards in a given card lst*)
let rec find_aces (lst : card list) acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if h.value = 11 then find_aces t (acc + 1) else find_aces t acc

(*[helper_value_of_cards lst] is the total value of a given hand in the
  form of a list of cards *)
let rec helper_value_of_cards (lst : card list) =
  match lst with [] -> 0 | h :: t -> h.value + helper_value_of_cards t

(*[change_value aces am] subtracts 10 from a given hand value am for
  every ace in the hand causing the hand to bust*)
let rec change_value (aces : int) (am : int) =
  if am > 21 && aces > 0 then change_value (aces - 1) (am - 10) else am

(**takes a card list and returns their values added*)
let rec value_of_cards (lst : card list) =
  change_value (find_aces lst 0) (helper_value_of_cards lst)

(**takes a card hand and adds a card to the back of the hand*)
let put_in_hand hand card =
  let nw_hand = List.rev (card :: List.rev hand.hand) in
  { hand with hand = nw_hand; value = value_of_cards nw_hand }

let count_card (crd : card) =
  if crd.value < 7 then 1 else if crd.value > 9 then -1 else 0

(** returns game with the top card dealt to a specific player*)
let rec deal_top_card (person : string) game =
  try
    let crd = top_card game.card_deck in
    {
      card_deck = deal_one game.card_deck;
      players_hand =
        (if person = "player" then put_in_hand game.players_hand crd
        else game.players_hand);
      split_hand = game.split_hand;
      dealers_hand =
        (if person = "dealer" then put_in_hand game.dealers_hand crd
        else game.dealers_hand);
      hi_lo_cnt = game.hi_lo_cnt + count_card crd;
      is_tutorial = false;
      deck_count = game.deck_count;
    }
  with EmptyDeck ->
    let temp_deck =
      {
        cards = game.card_deck.memory_cards;
        cards_left = List.length game.card_deck.memory_cards;
        memory_cards = game.card_deck.memory_cards;
      }
    in
    let ngame =
      {
        card_deck = temp_deck;
        players_hand = game.players_hand;
        split_hand = game.split_hand;
        dealers_hand = game.dealers_hand;
        is_tutorial = false;
        hi_lo_cnt = 0;
        deck_count = game.deck_count;
      }
    in
    deal_top_card person ngame

(** returns game with two cards dealt to the dealer and player*)
let deal game =
  deal_top_card "player" game
  |> deal_top_card "dealer" |> deal_top_card "player"
  |> deal_top_card "dealer"

(**the start of type game each round, which should contain the deck and
   both hands*)
let new_game game =
  {
    card_deck = shuffle_deck game.card_deck;
    players_hand =
      {
        hand = [];
        value = 0;
        person_type = Player;
        balance = game.players_hand.balance;
      };
    split_hand =
      {
        hand = [];
        value = 0;
        person_type = Player;
        balance = game.players_hand.balance;
      };
    dealers_hand =
      { hand = []; value = 0; person_type = Dealer; balance = 0 };
    hi_lo_cnt = game.hi_lo_cnt;
    is_tutorial = false;
    deck_count = game.deck_count;
  }

(**[create_game money cnt dck] creates a new game with a specific
   balance, money, out of a given number cnt of decks dck *)
let create_game money cnt dck =
  {
    card_deck = dck;
    players_hand =
      { hand = []; value = 0; person_type = Player; balance = money };
    split_hand =
      { hand = []; value = 0; person_type = Player; balance = money };
    dealers_hand =
      { hand = []; value = 0; person_type = Dealer; balance = 0 };
    hi_lo_cnt = 0;
    is_tutorial = false;
    deck_count = cnt;
  }

(**begins a new game and deals the cards out*)
let begin_game game = deal (new_game game)

(**adds a card to a hand and returns the new game*)
let hit game person = deal_top_card person game

(**checks hand and returns win condition*)
let check_condition hand =
  if hand.value < 21 then Under
  else if hand.value = 21 then Win
  else Bust

(*[is_blackjack hand] is true if a hand is a blackjack (face card +
  ace), false otherwise*)
let is_blackjack (hand : card list) =
  List.length hand = 2 && helper_value_of_cards hand = 21

(**[get_player_hand game] is the current hand of the player*)
let get_player_hand game = game.players_hand

(*The current valueof a given hand*)
let get_hand_value hand = hand.value

(*The player's current hand of a given game*)
let get_dealer_hand game = game.dealers_hand

(*The list of cards in a given current_hand*)
let get_cards current_hand = current_hand.hand

(*The value of a given card*)
let get_card_value (card : card) = card.value

(*The name of a given card*)
let get_card_name (card : card) = card.name

(*The suit of a given card*)
let get_card_suit (card : card) = card.suit

(*The balance of a given hand*)
let get_balance hand = hand.balance

(*[check_tutorial game] is true of the given game is a tutorial game,
  false otherwise*)
let check_tutorial game = game.is_tutorial

(*[is_split game] is true if a game has involved a Split action, false
  otherwise*)
let is_split game =
  match game.split_hand.hand with [] -> false | h :: _ -> true

(*[print_string_list lst] prints a list of strings from a given string
  list lst *)
let rec print_string_list lst =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      print_string h;
      print_string_list t

(*[print_hand hand] prints to the console a given hand*)
let print_hand (hand : current_hand) =
  match hand.person_type with
  | Player ->
      let cards_strings =
        List.map (fun (h : card) -> "" ^ h.name ^ "\n") hand.hand
      in
      let string_list =
        cards_strings
        @ [ ("Value: " ^ string_of_int hand.value) ^ "\n" ]
      in
      print_string_list string_list
  | Dealer -> (
      match hand.hand with
      | [] -> print_string ""
      | h :: _ ->
          print_string ("The Dealer is showing: " ^ h.name ^ "\n"))

(*[add_to_string str] concatenates a space to a given string if the
  string length is less than 35 characters. Otherwise, str is returned
  unmodified*)
let rec add_to_string str =
  if String.length str < 35 then add_to_string (str ^ " ") else str

(*[grow_string_list lst acc] *)
let rec grow_string_list lst acc =
  match lst with
  | [] -> acc
  | h :: t -> grow_string_list t (add_to_string h :: acc)

(*[combined hands plst dlst acc]*)
let rec combine_hands plst dlst acc =
  match plst with
  | [] -> List.rev acc
  | h :: t -> combine_hands t (List.tl dlst) ((h ^ List.hd dlst) :: acc)

(*[equal_lists lst1 lst2] *)
let rec equal_lists lst1 lst2 =
  if List.length lst1 < List.length lst2 then
    equal_lists ([ " " ] @ lst1) lst2
  else lst1

(*[dealer_shows gme] is the card that the dealer is showing in their
  hand in a given game gme*)
let dealer_shows gme =
  match gme.dealers_hand.hand with [] -> "" | h :: _ -> h.name

let print_cnt game bl tut_game cnt =
  if tut_game then
    match cnt with
    | HiLo ->
        let count =
          if bl then game.hi_lo_cnt
          else
            game.hi_lo_cnt
            -
            match List.rev game.dealers_hand.hand with
            | [] -> 0
            | hd :: _ ->
                if hd.value < 7 then 1
                else if hd.value > 9 then -1
                else 0
        in
        print_endline ("\nCurrent running total: " ^ string_of_int count)
    | NoMthd -> ()
  else ()

(*[print_both_hands game bl] prints the player's current hand, player's
  off hand (if it exists), and the dealer's hand in a stylized manner.*)
let print_both_hands game bl tut_game cnt =
  let phand = game.players_hand in
  let dhand = game.dealers_hand in
  let p_cards_strings =
    grow_string_list
      (equal_lists
         (helper_name_list phand.hand [])
         (helper_name_list dhand.hand []))
      []
  in
  let d_cards_string =
    if bl then
      List.rev
        (List.map
           (fun h -> h ^ "\n")
           (equal_lists
              (helper_name_list dhand.hand [])
              p_cards_strings))
    else
      List.rev
        (List.map
           (fun h -> h ^ "\n")
           (equal_lists
              (List.rev ([ dealer_shows game ] @ [ "( ? )" ]))
              p_cards_strings))
  in
  let bust =
    if dhand.value > 21 then "\nThe Dealer Busted! \n" else ""
  in
  let complete_string_list =
    combine_hands p_cards_strings d_cards_string []
    @
    if bl then
      [
        add_to_string ("\nValue: " ^ string_of_int phand.value)
        ^ " Value: "
        ^ string_of_int dhand.value
        ^ "\n" ^ bust;
      ]
    else
      [ add_to_string ("\nValue: " ^ string_of_int phand.value ^ "\n") ]
  in
  print_string_list complete_string_list;
  if is_split game then (
    print_newline ();
    print_endline "Off hand";
    print_hand game.split_hand);
  print_cnt game bl tut_game cnt

(*[get_cards_left game] is the number of cards remaining in the deck at
  a given state of the game*)
let get_cards_left game = game.card_deck.cards_left

(*[get_current_hand_card_names hnd] returns a list of names of cards in
  a given hand*)
let get_current_hand_card_names hnd = helper_name_list hnd.hand []

(*[set_balance game bal] adjusts the balance associated with game to
  bal*)
let set_balance game bal =
  let hand = { game.players_hand with balance = bal } in
  { game with players_hand = hand }

(*[add_to_balance sgame amt] adusts the balance associated with sgame by
  a given integer amt*)
let add_to_balance sgame amt =
  {
    sgame with
    players_hand =
      {
        sgame.players_hand with
        balance = sgame.players_hand.balance + amt;
      };
  }

(*[dealer_turn game stay blackjack value] simulates the rounds when the
  dealer begins drawing from the deck. Returns a result based on the
  condition of the dealer's final hand and the player's hand. For
  example: if the player loses, returns Lose. If the player wins returns
  Win.*)
let rec dealer_turn
    (game : game)
    (stay : bool)
    (blackjack : bool)
    (value : int)
    (tut_game : bool)
    (cnt : cnt_mthd) =
  let dealer_hand = { game.dealers_hand with person_type = Player } in
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nYour hand:                         ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "Dealer's hand:\n";
  print_both_hands game true tut_game cnt;
  let updated_game = { game with dealers_hand = dealer_hand } in
  match check_condition dealer_hand with
  | Under ->
      let dealer_value = value_of_cards dealer_hand.hand in
      if blackjack then Win
      else if dealer_value > 16 || value > 21 then
        if value > dealer_value && value <= 21 then Win
        else if value = dealer_value then Draw
        else Lose
      else
        let updated_game2 = hit updated_game "dealer" in
        dealer_turn updated_game2 stay blackjack value tut_game cnt
  | Win ->
      if is_blackjack (game.dealers_hand |> get_cards) then
        if blackjack then Draw else Lose
      else if blackjack then Win
      else if value = 21 then Draw
      else Lose
  | Bust -> Win

(*Checks game to see if insurance is possible*)
let check_insurance game =
  match game.dealers_hand.hand with
  | [] -> false
  | h :: t -> if h.rank = "Ace" then true else false

(*Checks game to see if double down is possible*)
let check_double_down game =
  match game.players_hand.hand with
  | [ h; h2 ] -> if not (is_split game) then true else false
  | _ -> false

(*Checks game to see if split is possible*)

let check_split game =
  match game.players_hand.hand with
  | h :: h2 :: t -> if h.rank = h2.rank then true else false
  | _ -> false

(*[flip_hands game] is the game state after the players_hand and the
  split_hand of the game are switched so that the player can continue
  playing the game with the off hand. *)
let flip_hands game =
  let phand = game.players_hand in
  let shand = game.split_hand in
  { game with players_hand = shand; split_hand = phand }

(*Checks to see if surrender is an option available to the player*)
let check_surrender game =
  if List.length game.players_hand.hand = 2 then true else false

(**)

(**A helper function with the same implementation as check_insurance for
   testing*)
let check_insurance_tester hand =
  match hand with
  | [] -> false
  | h :: t -> if h.rank = "Ace" then true else false

(**A helper function with the same implementation as check_double_down
   for testing*)
let check_double_down_tester hand =
  match hand with
  | [ h; h2 ] -> if h != h2 then true else false
  | _ -> false

(**A helper function with the same implementation as check_surrender for
   testing*)
let check_surrender_tester hand =
  if List.length hand = 2 then true else false

let add_action_string game f (s : string) (acc : string list) =
  if f game then s :: acc else acc

(*returns a string list of all possible actions for a player each
  turn *)
let possible_actions game =
  [ "Hit | "; "Stay | " ]
  @ ([ "End Blackjack\n" ]
    |> add_action_string game check_surrender "Surrender | "
    |> add_action_string game check_insurance "Insurance | "
    |> add_action_string game check_double_down "Double Down | "
    |> add_action_string game check_split "Split | ")

let rec list_to_spaced_string lst acc =
  match lst with
  | [] -> acc
  | h :: t -> list_to_spaced_string t (acc ^ h ^ "")

(*combines a string list into a string with breaks between each
  element*)
let action_string game =
  list_to_spaced_string (possible_actions game) ""

let rec helper_num_of_rank (lst : card list) rnk acc =
  match lst with
  | [] -> acc
  | h :: t ->
      if h.rank = rnk then helper_num_of_rank t rnk (acc + 1)
      else helper_num_of_rank t rnk acc

type number_of_ranks = {
  rank : string;
  number : int;
}

let rank_num rnk num = { rank = rnk; number = num }

let find_ranks_left gme rnk =
  helper_num_of_rank gme.card_deck.cards rnk 0

(*helper fxn that assists with removing the second card from a hand.
  Implemented to help split hands*)
let remove_sec_card game =
  match game.players_hand.hand with
  | h :: t ->
      let cards = h in
      {
        hand = [ cards ];
        value = helper_value_of_cards [ cards ];
        person_type = Player;
        balance = game.players_hand.balance;
      }
  | _ -> game.players_hand

(*Returns a split hand from a player's current hand provided that the
  current hand is eligible for split. *)
let make_split game =
  let hand = game.players_hand in
  if check_split game then
    match hand.hand with
    | h :: t ->
        let split =
          {
            hand = t;
            value = helper_value_of_cards t;
            person_type = Player;
            balance = game.players_hand.balance;
          }
        in
        {
          game with
          players_hand = remove_sec_card game;
          split_hand = split;
        }
    | _ -> game
  else (
    print_endline "Your cards do not allow you to do that ";
    game)

(*let end_game game hand win bet = let balance = get_balance hand in let
  new_game = set_balance (game |> new_game |> begin_game) balance in
  match (dealer_turn game win (is_blackjack (hand |> get_cards))
  (get_hand_value hand)) with | Lose -> print_endline "You lost! Would
  you like to play another hand?"; lost (read_line ()) (add_to_balance
  new_game (-bet)) | Win -> print_endline "You won! Would you like to
  play another hand?"; lost (read_line ()) (add_to_balance game (bet)) |
  Draw -> print_endline "You drew! Would you like to play another
  hand?"; lost (read_line ()) (add_to_balance game (0))*)
