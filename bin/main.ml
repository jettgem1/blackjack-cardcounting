open Game
open Yojson.Basic.Util
open Basegame
open Action

type tutorial = {
  introduction : string array;
  hit : string array;
  stay : string array;
  double_down : string array;
  insurance : string array;
  split : string array;
}

let is_tutorial = ref false

let intro_of_json json =
  [|
    json |> member "Welcome" |> to_string;
    json |> member "Betting" |> to_string;
    json |> member "Actions" |> to_string;
    json |> member "Objective" |> to_string;
    json |> member "Hands" |> to_string;
  |]

let actiont_of_json json =
  [|
    json |> member "First" |> to_string;
    json |> member "Repeating" |> to_string;
  |]

let tutorial_of_json json =
  {
    introduction = json |> member "Introduction" |> intro_of_json;
    hit = json |> member "Hit" |> actiont_of_json;
    stay = json |> member "Stay" |> actiont_of_json;
    double_down = json |> member "Double Down" |> actiont_of_json;
    insurance = json |> member "Insurance" |> actiont_of_json;
    split = json |> member "Split" |> actiont_of_json;
  }

let tut_print (arr : string array) first =
  if first then print_endline arr.(0) else print_endline arr.(1);
  ()

let deck_json = Yojson.Basic.from_file "./data/standard_deck.json"

(*let deck_json = Yojson.Basic.from_file "./data/insurance.json"*)

let deck = deck_of_json deck_json

let bj_tut =
  Yojson.Basic.from_file "./data/tutorial.json" |> tutorial_of_json

let tut_messages =
  [|
    bj_tut.hit.(0);
    bj_tut.stay.(0);
    bj_tut.double_down.(0);
    bj_tut.insurance.(0);
    bj_tut.split.(0);
  |]

let hi_lo_json = Yojson.Basic.from_file "./data/hi_lo_tut.json"

let hi_lo_begin =
  hi_lo_json
  |> Yojson.Basic.Util.member "beginning"
  |> Yojson.Basic.Util.to_string

let hi_lo_math_q =
  hi_lo_json
  |> Yojson.Basic.Util.member "math_q"
  |> Yojson.Basic.Util.to_string

let hi_lo_math =
  hi_lo_json
  |> Yojson.Basic.Util.member "math"
  |> Yojson.Basic.Util.to_string

let hi_lo_mult_q =
  hi_lo_json
  |> Yojson.Basic.Util.member "mult_q"
  |> Yojson.Basic.Util.to_string

let hi_lo_mult =
  hi_lo_json
  |> Yojson.Basic.Util.member "mult"
  |> Yojson.Basic.Util.to_string

let hi_lo_end =
  hi_lo_json
  |> Yojson.Basic.Util.member "end"
  |> Yojson.Basic.Util.to_string

let rec print_string_list lst =
  match lst with
  | [] -> print_endline ""
  | h :: t ->
      print_endline h;
      print_string_list t

let cards (c_hand : current_hand) = get_cards c_hand

let mthd = ref NoMthd

let tut_game = ref false

let rec start_value str =
  try
    let bet = int_of_string str in
    if bet > 0 && bet < 1000000000 then bet
    else if bet <= 0 then raise (Failure "value must be greater than 0")
    else if bet >= 1000000000 then
      raise (Failure "value must be smaller than 1 billion")
    else raise (Failure "invalid bet")
  with Failure str ->
    print_endline "Invalid input. Enter your starting value: ";
    let input = read_line () in
    start_value input

let rec check_decks str =
  try
    let amt = int_of_string str in
    if amt > 0 && amt <= 12 then amt
    else raise (Failure "invalid input")
  with Failure str ->
    print_endline "Invalid input. Deck amount must be between 1 and 12.";
    let input = read_line () in
    check_decks input

(*let print_hand (hand : current_hand) = let cards_strings = List.map
  (fun _( h : card) -> "Rank: " ^ get_card_name h ^ ", Suit : " ^
  h.suit) (Basegame.get_cards hand) in let string_list = ("Value: " ^
  (String.string_of_int hand.value)) :: cards_strings in
  print_string_list string_list *)

(*A reference to a boolean which corresponds to true if the player is
  done playing with the first hand of their split, \ false otherwise*)
let split_done = ref false

let rec act
    str
    (sgame : game)
    abet
    (arr : string array)
    (f : 'a -> unit) =
  try
    match read !is_tutorial tut_messages str with
    | End s ->
        if s = [ "blackjack" ] then (
          print_endline "Thanks for playing!\n";
          exit 0)
        else raise Malformed
    | Hit -> (
        let sgame = hit sgame "player" in
        let player_hand = get_player_hand sgame in
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "\nYour hand:                         ";
        ANSITerminal.print_string
          [ ANSITerminal.magenta ]
          "Dealer's hand:\n";
        print_both_hands sgame false !tut_game !mthd;
        match check_condition player_hand with
        | Under -> turn sgame abet
        | Win ->
            print_endline "\nYou got 21!\n";
            if is_split sgame && not !split_done then (
              split_done := true;
              let new_game = sgame |> flip_hands in
              turn new_game abet)
            else end_game sgame player_hand true abet
        | Bust ->
            if is_split sgame && not !split_done then (
              print_endline "First Hand busted!";
              split_done := true;
              let new_game = sgame |> flip_hands in
              turn new_game abet)
            else bust sgame player_hand abet)
    | Stay ->
        if is_split sgame && not !split_done then (
          split_done := true;
          let new_game = sgame |> flip_hands in
          turn new_game abet)
        else end_game sgame (get_player_hand sgame) true abet
    | Split ->
        let ngame = make_split sgame in
        ANSITerminal.print_string [ ANSITerminal.cyan ]
          "\nYour hand:                         ";
        ANSITerminal.print_string
          [ ANSITerminal.magenta ]
          "Dealer's hand:\n";
        print_both_hands ngame false !tut_game !mthd;
        print_endline "What would you like to do? \n";
        act (read_line ()) ngame abet [||] (fun () -> ())
    | Double_down ->
        if check_double_down sgame then (
          let sgame = hit sgame "player" in
          let player_hand = get_player_hand sgame in
          let dbet = abet * 2 in
          ANSITerminal.print_string [ ANSITerminal.cyan ]
            "\nYour hand:                         ";
          ANSITerminal.print_string
            [ ANSITerminal.magenta ]
            "Dealer's hand:\n";
          print_both_hands sgame false !tut_game !mthd;
          match check_condition player_hand with
          | Under | Win -> end_game sgame player_hand true dbet
          | Bust -> bust sgame player_hand dbet)
        else print_endline "\nYour cards do not allow you to do that \n";
        print_endline "What would you like to do? \n";
        act (read_line ()) sgame abet [||] (fun () -> ())
    | Insurance ->
        if check_insurance sgame then (
          print_endline "How much would you like to bet?\n";
          let curr_bal = sgame |> get_player_hand |> get_balance in
          let ins = bet (read_line ()) curr_bal in
          if is_blackjack (get_dealer_hand sgame |> get_cards) then (
            print_endline "\nA blackjack! You guessed right! \n";
            end_game sgame (get_player_hand sgame) true (abet - ins))
          else (
            print_endline
              "\nNot a blackjack. What would you like to do? \n";
            act (read_line ()) sgame (-abet + ins) [||] (fun () -> ())))
        else print_endline "\nYour cards do not allow you to do that \n";
        print_endline "What would you like to do? \n";
        act (read_line ()) sgame abet [||] (fun () -> ())
    | Surrender ->
        if check_surrender sgame then (
          print_endline
            "\n\
             You have surrendered your hand and will receive half of \
             your original bet\n";
          print_endline "Would you like to play another hand?";
          lost (read_line ()) (add_to_balance sgame (-(abet / 2))))
        else (
          print_endline "\nYour cards do not allow you to do that \n";
          print_endline "What would you like to do? \n";
          act (read_line ()) sgame abet [||] (fun () -> ()))
    | Start s ->
        print_endline
          "Cannot start the game while it is running, try again";
        act (read_line ()) sgame abet [||] (fun () -> ())
    | Bet s ->
        print_endline "Cannot bet in the middle of a hand, try again";
        act (read_line ()) sgame abet [||] (fun () -> ())
  with Malformed | End_of_file ->
    print_endline "Invalid input, try again";
    act (read_line ()) sgame abet [||] (fun () -> ())

and bust sgame hand bet =
  ANSITerminal.print_string [ ANSITerminal.cyan ] "\nYou Busted!\n";
  end_game sgame hand true bet

and lost str sgame =
  try
    match String.lowercase_ascii str with
    | "yes" | "y" -> start_game sgame (fun () -> ())
    | _ ->
        print_endline "Thanks for playing!\n";
        exit 0
  with End_of_file ->
    print_endline "Invalid input, try again";
    lost (read_line ()) sgame

and end_game game hand stay bet =
  let balance = get_balance hand in
  let ngame = set_balance (game |> begin_game) balance in
  let split_game = flip_hands game in
  match
    dealer_turn game stay
      (is_blackjack (hand |> get_cards))
      (get_hand_value hand) !tut_game !mthd
  with
  | Lose ->
      ANSITerminal.print_string [ ANSITerminal.red ] "\nYou lost!";
      if is_split game then (
        match
          dealer_turn split_game stay
            (split_game |> get_player_hand |> get_cards |> is_blackjack)
            (split_game |> get_player_hand |> get_hand_value)
            !tut_game !mthd
        with
        | Lose ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand lost!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame (-2 * bet))
        | Win ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand Won!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame 0)
        | Draw ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand drew!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame (-bet)))
      else (
        print_endline " Would you like to play another hand?\n";
        lost (read_line ()) (add_to_balance ngame (-bet)))
  | Win ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "\nYou won!";
      if is_split game then (
        match
          dealer_turn split_game stay
            (split_game |> get_player_hand |> get_cards |> is_blackjack)
            (split_game |> get_player_hand |> get_hand_value)
            !tut_game !mthd
        with
        | Lose ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand lost!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame 0)
        | Win ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand won!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame (2 * bet))
        | Draw ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand drew!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame bet))
      else (
        print_endline " Would you like to play another hand?\n";
        lost (read_line ()) (add_to_balance ngame bet))
  | Draw ->
      if is_split game then (
        match
          dealer_turn split_game stay
            (split_game |> get_player_hand |> get_cards |> is_blackjack)
            (split_game |> get_player_hand |> get_hand_value)
            !tut_game !mthd
        with
        | Lose ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand lost!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame (-bet))
        | Win ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand won!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame bet)
        | Draw ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\nYour other hand drew!";
            print_endline " Would you like to play another hand?\n";
            lost (read_line ()) (add_to_balance ngame 0))
      else (
        print_endline " Would you like to play another hand?\n";
        lost (read_line ()) (add_to_balance ngame 0))

and turn sgame (bet : int) =
  (*merge turn with act*)
  if is_blackjack (sgame |> get_player_hand |> get_cards) then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\nYou got a blackjack!\n";
    end_game sgame (sgame |> get_player_hand) true bet
    (*else if is_blackjack (sgame |> get_dealer_hand |> get_cards) then
      ( ANSITerminal.print_string [ ANSITerminal.magenta ] "\nThe dealer
      got a blackjack!\n"; print_endline "\nYou lost! Would you like to
      play another hand?"; lost (read_line ()) (add_to_balance sgame
      (-bet))) *))
  else (
    print_endline
      ("\nCards Left: " ^ string_of_int (get_cards_left sgame));
    (*print_endline ("Aces Left: " ^ string_of_int (find_ranks_left
      sgame "Ace"));*)
    print_endline "\nWhat would you like to do?\n";
    print_endline (action_string sgame);
    (*Hit\n\ Stay\n\ Split (WIP)\n\ Double Down (WIP)\n\ Insurance
      (WIP)\n\ End\n*)
    let input = read_line () in
    act input sgame bet [||] (fun () -> ()))

and bet str curr_bal =
  try
    let bet = int_of_string str in
    if bet > 0 && bet <= curr_bal then bet
    else raise (Failure "invalid bet")
  with Failure str ->
    print_endline "Invalid input. Enter bet amount : ";
    let input = read_line () in
    bet input curr_bal

and start_game sgame (f : unit -> unit) =
  let curr_bal = sgame |> get_player_hand |> get_balance in
  print_endline ("\nYour current balance is " ^ string_of_int curr_bal);
  if curr_bal <= 0 then (
    print_endline "You ran out of money. Thanks for playing!\n";
    exit 0);
  print_endline "How much would you like to bet?\n";
  let pbet = bet (read_line ()) curr_bal in
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\nYour hand:                         ";
  ANSITerminal.print_string [ ANSITerminal.magenta ] "Dealer's hand:\n";
  print_both_hands sgame false !tut_game !mthd;
  split_done := false;
  turn sgame pbet

let rec yn_check str =
  try
    match String.lowercase_ascii str with
    | "yes" | "y" -> true
    | "no" | "n" -> false
    | _ ->
        print_endline "Invalid input, try again";
        yn_check (read_line ())
  with End_of_file ->
    print_endline "Invalid input, try again";
    yn_check (read_line ())

let cc_tut_math () = print_endline hi_lo_math

let cc_tut_mult () = print_endline hi_lo_mult

let rec cc_tut_input f str =
  try
    match String.lowercase_ascii str with
    | "yes" | "y" -> f
    | _ ->
        print_endline "In that case, let us proceed with this tutorial."
  with End_of_file ->
    print_endline "Invalid input, try again";
    cc_tut_input f (read_line ())

let rec blackjack_tut (first : bool) () =
  let intro_length = Array.length bj_tut.introduction in
  if first then
    for x = 0 to intro_length - 1 do
      print_endline "Would you like to proceed with the tutorial (y/n)";
      if not (read_line () |> yn_check) then (
        print_endline
          "Type \"start blackjack\" to play a game or \"start \
           tutorial\" to begin the tutorial.\n\
           Type \"end blackjack\" to end the program at any time.";
        init (read_line ()));
      print_endline bj_tut.introduction.(x);
      print_newline ()
    done;
  print_endline "";
  print_endline "Would you like to proceed with the tutorial (y/n)";
  match String.lowercase_ascii (read_line ()) with
  | "yes" | "y" ->
      start_game
        (deck |> create_game 1000 1 |> begin_game)
        (fun () -> ())
  | "no" | "n" ->
      print_endline
        "Type \"start blackjack\" to play a game or \"start tutorial\" \
         to begin the tutorial.\n\
         Type \"end blackjack\" to end the program at any time.";
      init (read_line ())
  | "end blackjack" ->
      print_endline "Thanks for playing!\n";
      exit 0
  | _ ->
      print_endline "Invalid input. Try again";
      blackjack_tut false ()

and cc_tut () =
  print_endline hi_lo_begin;
  (*we ask whether or not the player wants to learn about the mathematic
    details of the running total*)
  print_endline hi_lo_math_q;
  cc_tut_input (cc_tut_math ()) (read_line ());
  (*we then ask whether or not the player wants to learn about what to
    do when there are multiple decks*)
  print_endline hi_lo_mult_q;
  cc_tut_input (cc_tut_mult ()) (read_line ());
  print_endline hi_lo_end;
  init (read_line ())

and start_tutorial () =
  try
    match String.lowercase_ascii (read_line ()) with
    | "b" | "blackjack" ->
        is_tutorial := true;
        blackjack_tut true ()
    | "c" | "card counting" -> cc_tut ()
    | "end" ->
        print_endline
          "Type \"start blackjack\" to play a game or \"start \
           tutorial\" to begin the tutorial.\n\
           Type \"end blackjack\" to end the program at any time.";
        init (read_line ())
    | _ ->
        print_endline "Invalid input, try again";
        start_tutorial ()
  with End_of_file ->
    print_endline "Invalid input, try again";
    start_tutorial ()

and choose_method sgame =
  try
    match String.lowercase_ascii (read_line ()) with
    | "hilo" | "hi-lo" | "hi lo" ->
        print_endline "Starting the game with the Hi-Lo helper...";
        mthd := HiLo;
        tut_game := true;
        start_game sgame (fun () -> ())
    | _ ->
        print_endline "Invalid input, starting with no helper...";
        mthd := NoMthd;
        tut_game := false;
        start_game sgame (fun () -> ())
  with End_of_file ->
    print_endline "Invalid input, starting with no helper...";
    mthd := NoMthd;
    tut_game := false;
    start_game sgame (fun () -> ())

and start_blackjack sgame =
  try
    match String.lowercase_ascii (read_line ()) with
    | "yes" | "y" ->
        print_endline
          "Which of the following card counting methods would you like \
           a helper for?\n\
           Hi-lo\n\
           (Other options to be implemented)";
        choose_method sgame
    | _ -> start_game sgame (fun () -> ())
  with End_of_file ->
    print_endline "Invalid input, try again";
    start_blackjack sgame

and init (str : string) =
  try
    match read !is_tutorial [||] str with
    | Start s ->
        if s = [ "blackjack" ] || s = [] then (
          is_tutorial := false;
          print_endline "\nHow much money do you want to spend today?\n";
          let start_val = start_value (read_line ()) in
          print_endline "\nHow many decks do you want in play?\n";
          let amt_decks = check_decks (read_line ()) in
          let sgame =
            set_deck deck amt_decks
            |> create_game start_val amt_decks
            |> begin_game
          in
          print_endline
            "Would you like to play using the card counting helper? \
             Type \"yes\" to do so.";
          start_blackjack sgame)
        else if s = [ "tutorial" ] then (
          print_endline
            "\n\
             Would you like to start the blackjack tutorial or the \
             card counting tutorial?\n\
             Type \"b\" for blackjack or \"c\" for card counting. Type \
             \"end\" to go back.";
          start_tutorial ())
        else print_endline "Invalid input, try again";
        init (read_line ())
    | End s ->
        print_endline "Thanks for playing!\n";
        exit 0
    | _ -> raise Malformed
  with Malformed | End_of_file ->
    print_endline "Invalid input, try again";
    init (read_line ())

let main () =
  print_string "\nWelcome to the ";
  ANSITerminal.print_string [ ANSITerminal.yellow ] "BLACKJACK";
  print_endline
    " simulator!\n\
     Type \"start blackjack\" to play a game. \n\
     Type \"start tutorial\" to begin a tutorial.\n\
     Type \"end blackjack\" to end the program at any time.\n";
  init (read_line ())

let () = main ()
