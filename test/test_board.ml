open Balatro

let fresh_board color = Board.create (Random.State.make [| 42 |]) color

(* [Card.t] does not derive [show], so its printer is written by hand. *)
let card =
  let pp fmt (c : Card.t) =
    Format.fprintf fmt "%s of %s" (Card.show_rank c.rank) (Card.show_suit c.suit)
  in
  Alcotest.testable pp ( = )
;;

let cards = Alcotest.list card
let error = Alcotest.testable Board.pp_error ( = )
let joker = Alcotest.testable Joker.pp Joker.equal

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Board.show_error e)
;;

let test_refill color =
  let board = fresh_board color in
  let original_cards = Deck.cards (Board.deck board) in
  Alcotest.(check int) "full deck" 52 (List.length original_cards);
  Alcotest.check cards "empty hand" [] (Board.hand board);
  let filled = Board.refill board in
  let drawn = Board.hand filled in
  let remaining = Deck.cards (Board.deck filled) in
  Alcotest.(check int) "drawn" 8 (List.length drawn);
  Alcotest.(check int) "remaining" 44 (List.length remaining);
  (* Every original card is in exactly one place: the hand or the deck. *)
  let same_card (a : Card.t) (b : Card.t) = a.suit = b.suit && a.rank = b.rank in
  List.iter
    (fun card ->
       let occurrences = List.filter (same_card card) (drawn @ remaining) in
       Alcotest.(check int) "card in one place" 1 (List.length occurrences))
    original_cards;
  (* Filling an already full hand must not consume more cards. *)
  let refilled = Board.refill filled in
  Alcotest.check cards "full hand is kept" drawn (Board.hand refilled);
  Alcotest.check cards "deck is kept" remaining (Deck.cards (Board.deck refilled));
  (* The original board remains unchanged. *)
  Alcotest.check cards "original hand" [] (Board.hand board);
  Alcotest.check cards "original deck" original_cards (Deck.cards (Board.deck board))
;;

let test_play_and_discard color =
  let board = fresh_board color |> Board.refill in
  let hand = Board.hand board in
  let after = ok (Board.discard [0; 2] board) in
  Alcotest.(check int) "hand after discard" 6 (List.length (Board.hand after));
  Alcotest.check
    cards
    "discarded"
    [List.nth hand 0; List.nth hand 2]
    (Board.discarded after);
  let refilled = Board.refill after in
  Alcotest.(check int) "refilled" 8 (List.length (Board.hand refilled));
  Alcotest.(check int) "deck" 42 (List.length (Deck.cards (Board.deck refilled)));
  let play, after = ok (Board.play [1] board) in
  Alcotest.check cards "scoring cards" [List.nth hand 1] (Play.scoring_cards play);
  Alcotest.(check int) "hand after play" 7 (List.length (Board.hand after))
;;

let test_invalid_selections color =
  let board = fresh_board color |> Board.refill in
  let expect_error indices expected =
    let check name = function
      | Error e -> Alcotest.check error name expected e
      | Ok _ -> Alcotest.fail (name ^ ": expected " ^ Board.show_error expected)
    in
    check "discard" (Board.discard indices board);
    check "play" (Board.play indices board)
  in
  expect_error [] Board.NoCards;
  expect_error [0; 1; 2; 3; 4; 5] (Board.TooManyCards 6);
  expect_error [8] (Board.InvalidIndex 8);
  expect_error [-1] (Board.InvalidIndex (-1));
  expect_error [3; 3] (Board.DuplicateIndex 3)
;;

(* Discarding and refilling until the deck runs out must not raise. *)
let test_empty_deck color =
  let rec loop board =
    match Deck.cards (Board.deck board) with
    | [] -> board
    | _ -> loop (Board.refill (ok (Board.discard [0; 1; 2; 3; 4] board)))
  in
  let board = fresh_board color |> Board.refill |> loop in
  (* No card was lost or duplicated along the way. *)
  Alcotest.(check int)
    "no card lost"
    52
    (List.length (Board.hand board) + List.length (Board.discarded board));
  (* Refilling with an empty deck leaves the hand as it is. *)
  Alcotest.check cards "hand is kept" (Board.hand board) (Board.hand (Board.refill board))
;;

(* [reset] puts every card back in the deck, shuffled, and keeps the jokers. *)
let test_reset color =
  let board = fresh_board color |> Board.refill in
  let board = Board.refill (ok (Board.discard [0; 1; 2] board)) in
  let board = ok (Board.add_joker Joker.Banner board) in
  let reset = Board.reset (Random.State.make [| 7 |]) board in
  Alcotest.check cards "empty hand" [] (Board.hand reset);
  Alcotest.check cards "nothing discarded" [] (Board.discarded reset);
  Alcotest.(check (list joker)) "jokers kept" [Joker.Banner] (Board.jokers reset);
  let deck = Deck.cards (Board.deck reset) in
  Alcotest.(check int) "full deck" 52 (List.length deck);
  let sorted cards = List.sort compare cards in
  let original = Deck.cards (Board.deck (fresh_board color)) in
  Alcotest.check cards "same cards" (sorted original) (sorted deck)
;;

let () =
  let color_name = function
    | Deck.Red -> "red"
    | Deck.Blue -> "blue"
    | Deck.Green -> "green"
  in
  let cases color =
    List.map
      (fun (name, test) -> Alcotest.test_case name `Quick (fun () -> test color))
      [
        "refill", test_refill;
        "play and discard", test_play_and_discard;
        "invalid selections", test_invalid_selections;
        "empty deck", test_empty_deck;
        "reset", test_reset;
      ]
  in
  let colors = [Deck.Red; Deck.Blue; Deck.Green] in
  Alcotest.run "Board" (List.map (fun color -> color_name color, cases color) colors)
;;
