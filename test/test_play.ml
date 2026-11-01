open Balatro

let hand = Alcotest.testable Poker.Hand.pp Poker.Hand.equal
let score = Alcotest.testable Score.pp Score.equal

let play cards =
  match Play.make cards with
  | Ok play -> play
  | Error e -> Alcotest.fail (Poker.Hand.show_error e)
;;

let check_score name levels cards ~chips ~mult =
  Alcotest.check score name (Score.create ~chips ~mult) (Play.score levels (play cards))
;;

let pair_of_aces = [Card.create Hearts Ace; Card.create Spades Ace]
let test_hand () = Alcotest.check hand "pair of aces" Pair (Play.hand (play pair_of_aces))

let test_score () =
  let levels = Poker.Levels.initial in
  (* 10 chips from the pair, 11 from each ace. *)
  check_score "aces are worth 11" levels pair_of_aces ~chips:32 ~mult:2.;
  (* Only the king scores: 5 + 10. *)
  check_score
    "only scoring cards count"
    levels
    [Card.create Hearts King; Card.create Spades Two]
    ~chips:15
    ~mult:1.;
  (* Face cards are worth 10: 30 + 9 + 10 + 10 + 10 + 10. *)
  check_score
    "face cards are worth 10"
    levels
    [
      Card.create Clubs Nine;
      Card.create Diamonds Ten;
      Card.create Hearts Jack;
      Card.create Spades Queen;
      Card.create Clubs King;
    ]
    ~chips:79
    ~mult:4.
;;

(* A pair at level 2 scores 25 chips and 3 mult before the cards. *)
let test_levels () =
  let levels = Poker.Levels.use_planet Consumables.Planet.Mercury Poker.Levels.initial in
  check_score "pair at level 2" levels pair_of_aces ~chips:47 ~mult:3.
;;

let test_too_many_cards () =
  let six_cards = List.init 6 (fun _ -> Card.create Hearts Two) in
  Alcotest.(check bool) "six cards" true (Result.is_error (Play.make six_cards))
;;

let () =
  Alcotest.run
    "Play"
    [
      ( "play",
        [
          Alcotest.test_case "hand" `Quick test_hand;
          Alcotest.test_case "score" `Quick test_score;
          Alcotest.test_case "levels" `Quick test_levels;
          Alcotest.test_case "too many cards" `Quick test_too_many_cards;
        ] );
    ]
;;
