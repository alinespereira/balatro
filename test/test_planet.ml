open Balatro
open Balatro.Base
open Balatro.Base.Consumables
module HandScoring = Balatro.Scoring.Hand

let () =
  let cases =
    [
      Planet.Pluto, Poker_hand.HighCard, (10, 1.);
      Planet.Mercury, Poker_hand.Pair, (15, 1.);
      Planet.Uranus, Poker_hand.TwoPair, (20, 1.);
      Planet.Venus, Poker_hand.ThreeOfAKind, (20, 2.);
      Planet.Saturn, Poker_hand.Straight, (30, 3.);
      Planet.Jupiter, Poker_hand.Flush, (15, 2.);
      Planet.Earth, Poker_hand.FullHouse, (25, 2.);
      Planet.Mars, Poker_hand.FourOfAKind, (30, 3.);
      Planet.Neptune, Poker_hand.StraightFlush, (40, 4.);
      Planet.Neptune, Poker_hand.RoyalFlush, (40, 4.);
    ]
  in
  List.iter
    (fun (planet, hand, (chips, mult)) ->
       assert (HandScoring.score planet hand = [Score.AddChips chips; Score.AddMult mult]))
    cases;
  assert (HandScoring.score Planet.Pluto Poker_hand.Pair = []);
  assert (HandScoring.score Planet.Mercury Poker_hand.HighCard = []);
  assert (HandScoring.score Planet.Neptune Poker_hand.FlushFive = []);
  let original = List.assoc Poker_hand.FullHouse HandScoring.base_hands in
  let upgraded = HandScoring.consume Planet.Earth original in
  assert (upgraded.level = 2);
  assert (Score.equal upgraded.scoring_effect (Score.create ~chips:65 ~mult:6.));
  let upgraded_again = HandScoring.consume Planet.Earth upgraded in
  assert (upgraded_again.level = 3);
  assert (Score.equal upgraded_again.scoring_effect (Score.create ~chips:90 ~mult:8.));
  assert (HandScoring.consume Planet.Pluto original = original);
  assert (original.level = 1);
  assert (Score.equal original.scoring_effect (Score.create ~chips:40 ~mult:4.));
  assert (
    Score.equal
      (Score.apply (Score.TimesMult 1.5) (Score.create ~chips:40 ~mult:4.))
      (Score.create ~chips:40 ~mult:6.))
;;
