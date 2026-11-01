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

let pair_of_kings = play Card.[create Spades King; create Hearts King]

let straight =
  play
    Card.
      [
        create Spades Five;
        create Hearts Six;
        create Clubs Seven;
        create Diamonds Eight;
        create Spades Nine;
      ]
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

(* A joker with state scores what its state says. *)
let test_stateful_changes () =
  let one joker = Joker.changes (context pair_of_hearts) joker in
  check ~expected_chips:0 ~expected_mult:3. (one (Joker.GreenJoker { mult = 3 }));
  check ~expected_chips:0 ~expected_mult:0. (one (Joker.GreenJoker { mult = 0 }));
  check ~expected_chips:0 ~expected_mult:2. (one (Joker.RideTheBus { mult = 2 }));
  check ~expected_chips:30 ~expected_mult:0. (one (Joker.Runner { chips = 30 }));
  check ~expected_chips:100 ~expected_mult:0. (one (Joker.IceCream { chips = 100 }))
;;

let reacted = Alcotest.option joker

let check_react name event before expected =
  Alcotest.check reacted name expected (Joker.react event before)
;;

let test_react () =
  let played p = Joker.Played p in
  let green mult = Joker.GreenJoker { mult } in
  check_react "green: play" (played pair_of_hearts) (green 0) (Some (green 1));
  check_react "green: discard" Joker.Discarded (green 2) (Some (green 1));
  check_react "green: never below 0" Joker.Discarded (green 0) (Some (green 0));
  let bus mult = Joker.RideTheBus { mult } in
  check_react "bus: no face card" (played pair_of_hearts) (bus 2) (Some (bus 3));
  check_react "bus: scoring face card" (played pair_of_kings) (bus 2) (Some (bus 0));
  (* The king of a high card with an ace does not score, so the bus goes on. *)
  check_react
    "bus: face card that does not score"
    (played high_card)
    (bus 2)
    (Some (bus 3));
  check_react "bus: discard" Joker.Discarded (bus 2) (Some (bus 2));
  let runner chips = Joker.Runner { chips } in
  check_react "runner: straight" (played straight) (runner 15) (Some (runner 30));
  check_react
    "runner: no straight"
    (played pair_of_hearts)
    (runner 15)
    (Some (runner 15));
  check_react "runner: discard" Joker.Discarded (runner 15) (Some (runner 15));
  let ice_cream chips = Joker.IceCream { chips } in
  check_react
    "ice cream: play"
    (played pair_of_hearts)
    (ice_cream 100)
    (Some (ice_cream 95));
  check_react "ice cream: melts" (played pair_of_hearts) (ice_cream 5) None;
  check_react "ice cream: discard" Joker.Discarded (ice_cream 100) (Some (ice_cream 100));
  (* Jokers without state come back as they are. *)
  List.iter
    (fun j ->
       check_react (Joker.show j ^ ": play") (played straight) j (Some j);
       check_react (Joker.show j ^ ": discard") Joker.Discarded j (Some j))
    Joker.[Joker; GreedyJoker; JollyJoker; HalfJoker; Banner; AbstractJoker; Cavendish]
;;

let test_fresh_and_same_kind () =
  (* The shop sells every joker fresh, and each one only once. *)
  List.iter
    (fun j ->
       Alcotest.check joker (Joker.show j ^ " is fresh") (Joker.fresh j) j;
       Alcotest.(check int)
         (Joker.show j ^ " is listed once")
         1
         (List.length (List.filter (Joker.same_kind j) Joker.all)))
    Joker.all;
  Alcotest.check
    joker
    "green joker back to 0"
    (Joker.GreenJoker { mult = 0 })
    (Joker.fresh (Joker.GreenJoker { mult = 5 }));
  Alcotest.check
    joker
    "ice cream back to 100"
    (Joker.IceCream { chips = 100 })
    (Joker.fresh (Joker.IceCream { chips = 20 }));
  let same name expected a b =
    Alcotest.(check bool) name expected (Joker.same_kind a b)
  in
  same
    "green jokers"
    true
    (Joker.GreenJoker { mult = 5 })
    (Joker.GreenJoker { mult = 0 });
  same
    "runner and ice cream"
    false
    (Joker.Runner { chips = 0 })
    (Joker.IceCream { chips = 0 });
  same "joker and joker" true Joker.Joker Joker.Joker;
  same "joker and banner" false Joker.Joker Joker.Banner
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
          Alcotest.test_case "stateful changes" `Quick test_stateful_changes;
          Alcotest.test_case "react" `Quick test_react;
          Alcotest.test_case "fresh and same kind" `Quick test_fresh_and_same_kind;
          Alcotest.test_case "order" `Quick test_order;
          Alcotest.test_case "sell value" `Quick test_sell_value;
          Alcotest.test_case "board slots" `Quick test_board_slots;
        ] );
    ]
;;
