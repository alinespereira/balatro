open Balatro

let play cards =
  match Play.make cards with
  | Ok play -> play
  | Error _ -> failwith "not a valid play"
;;

let check_score levels cards ~chips ~mult =
  let score = Play.score levels (play cards) in
  assert (Score.equal score (Score.create ~chips ~mult))
;;

let () =
  let levels = Poker.Levels.initial in
  let pair_of_aces = [Card.create Hearts Ace; Card.create Spades Ace] in
  assert (Poker.Hand.equal (Play.hand (play pair_of_aces)) Pair);
  (* 10 chips from the pair, 11 from each ace. *)
  check_score levels pair_of_aces ~chips:32 ~mult:2.;
  (* Only the king scores: 5 + 10. *)
  check_score levels [Card.create Hearts King; Card.create Spades Two] ~chips:15 ~mult:1.;
  (* Face cards are worth 10: 30 + 9 + 10 + 10 + 10 + 10. *)
  check_score
    levels
    [
      Card.create Clubs Nine;
      Card.create Diamonds Ten;
      Card.create Hearts Jack;
      Card.create Spades Queen;
      Card.create Clubs King;
    ]
    ~chips:79
    ~mult:4.;
  (* A pair at level 2 scores 25 chips and 3 mult before the cards. *)
  let levels = Poker.Levels.use_planet Consumables.Planet.Mercury levels in
  check_score levels pair_of_aces ~chips:47 ~mult:3.;
  let six_cards = List.init 6 (fun _ -> Card.create Hearts Two) in
  assert (Result.is_error (Play.make six_cards))
;;
