open Balatro

let play cards =
  match Play.make cards with
  | Ok play -> play
  | Error e -> Alcotest.fail (Poker.Hand.show_error e)
;;

let pair_of_diamonds =
  play Card.[create Diamonds Ace; create Diamonds Ace; create Diamonds King]
;;

let pair_of_hearts = play Card.[create Hearts Ace; create Spades Ace]

let two_pair =
  play Card.[create Spades Ace; create Hearts Ace; create Spades King; create Hearts King]
;;

let three_of_a_kind = play Card.[create Spades Ace; create Hearts Ace; create Clubs Ace]
let high_card = play Card.[create Spades Ace; create Hearts King]

(* A pair played with two kickers: four cards played, two of them score. *)
let pair_with_kickers =
  play Card.[create Spades Ace; create Hearts Ace; create Clubs Two; create Clubs Five]
;;

let context ?(discards_left = 0) ?(jokers = []) play =
  { Joker.play; Joker.discards_left; Joker.jokers }
;;

(* Applies the changes to a score of [chips] and [mult], so the tests check what a
   joker adds up to and not how it splits it into changes. *)
let scored ?(chips = 0) ?(mult = 0.) changes =
  Score.apply_all changes (Score.create ~chips ~mult)
;;

let score = Alcotest.testable Score.pp Score.equal
let joker = Alcotest.testable Joker.pp Joker.equal

let check ?chips ?mult ~expected_chips ~expected_mult changes =
  Alcotest.check
    score
    "score"
    (Score.create ~chips:expected_chips ~mult:expected_mult)
    (scored ?chips ?mult changes)
;;

let test_changes () =
  let one ?(ctx = context pair_of_hearts) joker = Joker.changes ctx joker in
  check ~expected_chips:0 ~expected_mult:4. (one Joker.Joker);
  (* Only the scoring cards count: the king of diamonds does not score in a pair. *)
  check
    ~expected_chips:0
    ~expected_mult:6.
    (one ~ctx:(context pair_of_diamonds) Joker.GreedyJoker);
  check
    ~expected_chips:0
    ~expected_mult:0.
    (one ~ctx:(context pair_of_diamonds) LustyJoker);
  check ~expected_chips:0 ~expected_mult:3. (one LustyJoker);
  check ~expected_chips:0 ~expected_mult:3. (one WrathfulJoker);
  check ~expected_chips:0 ~expected_mult:0. (one GluttonousJoker);
  check ~expected_chips:0 ~expected_mult:8. (one JollyJoker);
  (* Jolly Joker counts every hand that contains a pair, not only [Pair]. *)
  check ~expected_chips:0 ~expected_mult:8. (one ~ctx:(context two_pair) JollyJoker);
  check
    ~expected_chips:0
    ~expected_mult:8.
    (one ~ctx:(context three_of_a_kind) JollyJoker);
  check ~expected_chips:0 ~expected_mult:0. (one ~ctx:(context high_card) JollyJoker);
  check
    ~expected_chips:0
    ~expected_mult:20.
    (one ~ctx:(context pair_of_diamonds) HalfJoker);
  check ~expected_chips:0 ~expected_mult:0. (one ~ctx:(context two_pair) HalfJoker);
  (* Half Joker counts every card played, not only the ones that score. *)
  check
    ~expected_chips:0
    ~expected_mult:0.
    (one ~ctx:(context pair_with_kickers) HalfJoker);
  check
    ~expected_chips:60
    ~expected_mult:0.
    (one ~ctx:(context ~discards_left:2 pair_of_hearts) Banner);
  check
    ~expected_chips:0
    ~expected_mult:9.
    (one
       ~ctx:(context ~jokers:Joker.[Joker; Banner; AbstractJoker] pair_of_hearts)
       AbstractJoker);
  check ~mult:2. ~expected_chips:0 ~expected_mult:6. (one Cavendish)
;;

(* Jokers score left to right, so +4 before x3 is not the same as x3 before +4. *)
let test_order () =
  let all jokers = Joker.all_changes (context ~jokers pair_of_hearts) in
  check ~mult:1. ~expected_chips:0 ~expected_mult:15. (all Joker.[Joker; Cavendish]);
  check ~mult:1. ~expected_chips:0 ~expected_mult:7. (all Joker.[Cavendish; Joker]);
  check ~mult:1. ~expected_chips:0 ~expected_mult:1. (all [])
;;

let test_sell_value () =
  Alcotest.(check int) "joker" 1 (Joker.sell_value Joker.Joker);
  Alcotest.(check int) "greedy joker" 2 (Joker.sell_value Joker.GreedyJoker);
  List.iter
    (fun joker ->
       let value = Joker.sell_value joker in
       Alcotest.(check bool)
         (Joker.show joker ^ " sells for less than it costs")
         true
         (value >= 1 && value < Joker.cost joker))
    Joker.all
;;

let test_board_slots () =
  let ok = function
    | Ok board -> board
    | Error e -> Alcotest.fail (Board.show_error e)
  in
  let board = Board.create (Random.State.make [| 42 |]) Deck.Red in
  let five = Joker.[Joker; GreedyJoker; Banner; HalfJoker; Cavendish] in
  let full =
    List.fold_left (fun board joker -> ok (Board.add_joker joker board)) board five
  in
  Alcotest.(check (list joker)) "five jokers" five (Board.jokers full);
  Alcotest.(check bool)
    "no sixth slot"
    true
    (Board.add_joker Joker.JollyJoker full = Error Board.NoJokerSlot);
  let removed, after = ok (Board.remove_joker 1 full) in
  Alcotest.check joker "removed" Joker.GreedyJoker removed;
  Alcotest.(check (list joker))
    "jokers left"
    Joker.[Joker; Banner; HalfJoker; Cavendish]
    (Board.jokers after);
  Alcotest.(check bool) "index 5" true (Result.is_error (Board.remove_joker 5 full));
  Alcotest.(check bool) "index -1" true (Result.is_error (Board.remove_joker (-1) full))
;;

let () =
  Alcotest.run
    "Joker"
    [
      ( "joker",
        [
          Alcotest.test_case "changes" `Quick test_changes;
          Alcotest.test_case "order" `Quick test_order;
          Alcotest.test_case "sell value" `Quick test_sell_value;
          Alcotest.test_case "board slots" `Quick test_board_slots;
        ] );
    ]
;;
