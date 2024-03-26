open OUnit2
open Game
open Basegame
open Action

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. - COPIED FROM A2 AND NOT
    COUNTED IN LINES OF CODE*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. - COPIED FROM A2 AND NOT COUNTED
    IN LINES OF CODE*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let deck_1 =
  Yojson.Basic.from_file "data/standard_deck.json" |> deck_of_json

let deck_2 =
  Yojson.Basic.from_file "data/small_deck.json" |> deck_of_json

let deck_3 =
  Yojson.Basic.from_file "data/double_down_deck.json" |> deck_of_json

let deck_1_names = all_cards deck_1

let deck_2_names = all_cards deck_2

let game_test =
  deck_2 |> create_game 1000 1 |> begin_game |> get_player_hand

let test_shuffle =
  [
    ( "shuffle deck_1 once" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        deck_1_names
        (all_cards (shuffle_deck deck_1)) );
    ( "shuffle deck_1 twice" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        deck_1_names
        (all_cards (shuffle_deck (shuffle_deck deck_1))) );
    ( "shuffle deck_2" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        deck_2_names
        (all_cards (shuffle_deck deck_2)) );
  ]

let test_card_ace = "Ace of Hearts"

let deck_player_starting_hand = [ "Three of Hearts"; "Ace of Hearts" ]

let deck_dealers_starting_hand = [ "Four of Hearts"; "Two of Hearts" ]

let empty_hand = []

let top_card_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (top_card input |> get_card_name)

let test_top_card =
  [
    top_card_test "test top_card on deck specified by small_deck.json"
      test_card_ace deck_2;
    top_card_test
      "test top_card on deck specified by standard_deck.json"
      test_card_ace deck_1;
  ]

let helper_num_of_cards name expected_output funct input =
  name >:: fun _ ->
  assert_equal expected_output
    (get_cards_left (funct (create_game 1000 1 input)))

let helper_test_name_of_cards name expected_output funct input1 input2 =
  name >:: fun _ ->
  assert_equal expected_output
    (input1 |> create_game 1000 |> funct |> input2
   |> get_current_hand_card_names)

let helper_check_condition name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output
    (input |> create_game 1000 1 |> get_player_hand |> check_condition)

let hit_helper name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output
    (hit (create_game 1000 1 input) "player"
    |> get_player_hand |> check_condition)

let hit_bust name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output
    (hit
       (hit
          (hit
             (hit
                (hit
                   (hit
                      (hit (create_game 1000 1 input) "player")
                      "player")
                   "player")
                "player")
             "player")
          "player")
       "player"
    |> get_player_hand |> check_condition)

let hit_tests =
  [
    hit_helper "test hit when player hand is empty and deck is deck2"
      Under deck_2;
    hit_helper "test hit when player hand is empty and deck is deck1"
      Under deck_1;
    hit_bust "test multiple hits with deck2 until player busts" Bust
      deck_2;
    hit_bust "test multiple hits with deck1 and player is under" Bust
      deck_1;
  ]

let test_deal =
  [
    helper_num_of_cards
      "test deal on small_deck.json without shuffling using number of \
       cards left in the deck"
      4 deal deck_2;
    helper_num_of_cards
      "test deal on standard_deck.json without shuffling using number \
       of cards left in the deck"
      48 deal deck_1;
  ]

let helper_possible_actions name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output
    (possible_actions (deal (create_game 1000 1 input)))

let test_possible_actions =
  [
    helper_possible_actions "test double down on small deck"
      [ "Hit"; "Stay"; "End" ]
      deck_2;
  ]

(** let hit_test = Hit

    let stay_test = Stay

    let split_test = Split

    let double_down_test = Double_down*)
let bet_test_10 = Bet [ "10" ]

let insurance_test = Insurance

let start_test = Start [ "random" ]

let end_test = End [ "random" ]
(* let king_of_hearts = {"king";"hearts";12}

   let king_of_diamonds = {"king";"diamonds";12}

   let king_of_spades = {"king";"spades";12}

   let king_of_clubs = {"king";"clubs";12}

   let two_of_hearts ={"two";"hearts";2}

   let read_exception_tests name expected_exception input = name >:: fun
   _ -> assert_raises expected_exception (read input)

   let value_of_cards_test name expected_output input = name >:: fun _
   -> assert_equal expected_output (value_of_cards input) *)

let make_read_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (read false [||] input)

let make_check_insurance_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (input |> check_insurance_tester)

let make_check_double_down name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (check_double_down_tester input)

let make_check_surrender name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (check_surrender_tester input)

let insurance_hand = [ "Ace of Hearts"; "Three of Spades" ]

let split_hand = [ "Ace of Hearts"; "Ace of Spades" ]

let test_check_double_down =
  [
    make_check_double_down "test a hand that is split" true split_hand;
    make_check_double_down "test an insurance hand" true insurance_hand;
    make_check_double_down "test an empty hand" false empty_hand;
    make_check_double_down "test a random hand that is not split" true
      deck_player_starting_hand;
    make_check_double_down "test a random hand that is not split" true
      deck_dealers_starting_hand;
  ]

let test_check_insurance =
  [ (*make_check_insurance_test "test to see if check insurance returns
      false with empty hand" false empty_hand; make_check_insurance_test
      "test to see if check inurance returns true with right hand" true
      insurance_hand; make_check_insurance_test "test random hand" false
      deck_player_starting_hand; make_check_insurance_test "test random
      hand" false deck_dealers_starting_hand; make_check_insurance_test
      "test gibberish" false ["gibbersh"];*) ]

let test_surrender =
  [
    make_check_surrender "test with insurance hand" true insurance_hand;
    make_check_surrender "test with player starting hand" true
      deck_player_starting_hand;
    make_check_surrender "test with dealer starting hand" true
      deck_dealers_starting_hand;
    make_check_surrender "test with empty hand" false empty_hand;
    make_check_surrender "test with a large hand" false
      [ "Ace of Hearts"; "Three of Spades"; "Four of Hearts" ];
  ]

let test_read =
  [
    make_read_test "test a bet of value 10" bet_test_10 "bet 10";
    make_read_test "test a bet of value 10 with a lot of white spaces"
      bet_test_10 "      bet   10 ";
    make_read_test "test a hit action with a lot of white spaces" Hit
      "   hit       ";
    make_read_test "test a stay action" Stay "stay";
    make_read_test "test a stay action with a lot of white spaces" Stay
      "        stay ";
    make_read_test "test a split action" Split "split";
    make_read_test "test a split action with a lot of white spaces"
      Split "  split  ";
    make_read_test "test a double down action" Double_down "double down";
    make_read_test
      "test a double down action with a lot of white spaces" Double_down
      "   double down   ";
    (*make_read_test "test a random insurance action" insurance_test
      "insurance random";*)
    (*make_read_test "test a random insurance action with a lot of white
      spaces" insurance_test " insurance random ";*)
    make_read_test "test a random start action" start_test
      "start random";
    make_read_test
      "test a random start action with a lot of white spaces" start_test
      "start random    ";
    make_read_test "test a random end action" end_test "end random";
    make_read_test "test a random end action with a lot of white spaces"
      end_test "    end random";
    (* read_exception_tests "test an empty action" Empty "";
       read_exception_tests "test a bet with no values specified"
       Malformed " Bet "; read_exception_tests "test a bet with a value
       that does not make sense" Malformed "Bet my life";
       read_exception_tests "test a hit with some random value"
       Malformed " Hit 10"; read_exception_tests "test a hit with
       gibberish attached" Malformed "Hit agreghoqug ";
       read_exception_tests "test a stay with some random value"
       Malformed " Stay 10"; read_exception_tests "test a stay with
       gibberish attached" Malformed "Stay rethbgbrt ";
       read_exception_tests "test a split with some random value"
       Malformed " Split 10"; read_exception_tests "test a split with
       gibberish attached" Malformed "Split rethbgbrt ";
       read_exception_tests "test a false double down action" Malformed
       " Double Up"; read_exception_tests "test a double down action
       with random value" Malformed " Double down 50";
       read_exception_tests "test a double down action with gibberish
       attached" Malformed "double down lets gooo"; read_exception_tests
       "test empty insurance action" Malformed "insurance";
       read_exception_tests "test empty start action" Malformed "start";
       read_exception_tests "test empty end action" Malformed "end"; *)
    ( "test an empty action" >:: fun _ ->
      assert_raises Empty (fun _ -> read false [||] "") );
    ( "test a bet with no values specified" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] " Bet  ") );
    ( "test a hit with some random value" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] "  Hit 10 ") );
    ( "test a hit with gibberish attached" >:: fun _ ->
      assert_raises Malformed (fun _ ->
          read false [||] "  Hit dsgwergq6ign ") );
    ( "test a split with some random value" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] "  Split 10 ")
    );
    ( "test a split with gibberish attached" >:: fun _ ->
      assert_raises Malformed (fun _ ->
          read false [||] "  Split lq3qimfwk ") );
    ( "test a false double down action" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] "  double up")
    );
    ( "test a double down action with random value" >:: fun _ ->
      assert_raises Malformed (fun _ ->
          read false [||] "  Double down    50") );
    ( "test a double down action with gibberish attached" >:: fun _ ->
      assert_raises Malformed (fun _ ->
          read false [||] "  Double down lets goooo") );
    (*( "test an empty insurance action" >:: fun _ -> assert_raises
      Malformed (fun _ -> read false [||] "insurance") );*)
    ( "test an empty start action" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] "start") );
    ( "test an empty end action" >:: fun _ ->
      assert_raises Malformed (fun _ -> read false [||] "end") );
    ( "test an arbitrary string" >:: fun _ ->
      assert_raises Malformed (fun _ ->
          read false [||] "arbitrary string") );
  ]

(* let test_value_of_cards = [ value_of_cards_test "test an empty list"
   0 []; value_of_cards_test "test a list of one random card" 12 [
   king_of_hearts ]; value_of_cards_test "test a list of another random
   card" 2 [ two_of_hearts ]; value_of_cards_test "test a list of two
   kings of different suits" 24 [ king_of_clubs; king_of_diamonds ];
   value_of_cards_test "test a list of three kings of different suits"
   36 [ king_of_spades; king_of_hearts; king_of_clubs ];
   value_of_cards_test "test a list of four kings of different suits" 48
   [ king_of_clubs; king_of_diamonds; king_of_hearts; king_of_spades; ];
   value_of_cards_test "test a combination of two different cards" 14 [
   king_of_clubs; two_of_hearts ]; ] *)
let suite =
  "test suite for Blackjack"
  >::: List.flatten
         [
           test_read;
           test_shuffle;
           test_top_card;
           test_deal;
           hit_tests;
           test_possible_actions;
           test_check_double_down;
           test_check_insurance;
         ]

let _ = run_test_tt_main suite
