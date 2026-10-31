open Balatro

let play cards =
  match Play.make cards with
  | Ok play -> play
  | Error e -> failwith (Poker.Hand.show_error e)
;;

let pair_of_diamonds =
  play Card.[create Diamonds Ace; create Diamonds Ace; create Diamonds King]
;;

let pair_of_hearts = play Card.[create Hearts Ace; create Spades Ace]

let two_pair =
  play Card.[create Spades Ace; create Hearts Ace; create Spades King; create Hearts King]
;;

let context ?(discards_left = 0) ?(jokers = []) play =
  { Joker.play; Joker.discards_left; Joker.jokers }
;;

(* Applies the changes to a score of [chips] and [mult], so the tests check what a
   joker adds up to and not how it splits it into changes. *)
let scored ?(chips = 0) ?(mult = 0.) changes =
  Score.apply_all changes (Score.create ~chips ~mult)
;;

let check ?chips ?mult ~expected_chips ~expected_mult changes =
  let score = scored ?chips ?mult changes in
  assert (score.Score.chips = expected_chips);
  assert (Float.equal score.Score.mult expected_mult)
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
  check ~expected_chips:0 ~expected_mult:0. (one ~ctx:(context two_pair) JollyJoker);
  check
    ~expected_chips:0
    ~expected_mult:20.
    (one ~ctx:(context pair_of_diamonds) HalfJoker);
  check ~expected_chips:0 ~expected_mult:0. (one ~ctx:(context two_pair) HalfJoker);
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
  assert (Joker.sell_value Joker.Joker = 1);
  assert (Joker.sell_value Joker.GreedyJoker = 2);
  List.iter
    (fun joker ->
       assert (Joker.sell_value joker >= 1 && Joker.sell_value joker < Joker.cost joker))
    Joker.all
;;

let test_board_slots () =
  let ok = function
    | Ok board -> board
    | Error e -> failwith (Board.show_error e)
  in
  let board = Board.create (Random.State.make [| 42 |]) Deck.Red in
  let full =
    List.fold_left
      (fun board joker -> ok (Board.add_joker joker board))
      board
      Joker.[Joker; GreedyJoker; Banner; HalfJoker; Cavendish]
  in
  assert (Board.jokers full = Joker.[Joker; GreedyJoker; Banner; HalfJoker; Cavendish]);
  assert (Board.add_joker Joker.JollyJoker full = Error Board.NoJokerSlot);
  (match Board.remove_joker 1 full with
   | Ok (joker, after) ->
     assert (joker = Joker.GreedyJoker);
     assert (Board.jokers after = Joker.[Joker; Banner; HalfJoker; Cavendish])
   | Error e -> failwith (Board.show_error e));
  assert (Result.is_error (Board.remove_joker 5 full));
  assert (Result.is_error (Board.remove_joker (-1) full))
;;

let () =
  test_changes ();
  test_order ();
  test_sell_value ();
  test_board_slots ()
;;
