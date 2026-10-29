open Balatro
open Balatro.Base
open Balatro.Base.Consumables
module Effect = Balatro.Base.Effect
module EffectScoring = Balatro.Scoring.Effect
module HandScoring = Balatro.Scoring.Hand

let () =
  let cases =
    [
      Planet.Pluto, Hand.High_card, (10, 1);
      Planet.Mercury, Hand.Pair, (15, 1);
      Planet.Uranus, Hand.Two_pair, (20, 1);
      Planet.Venus, Hand.Three_of_a_kind, (20, 2);
      Planet.Saturn, Hand.Straight, (30, 3);
      Planet.Jupiter, Hand.Flush, (15, 2);
      Planet.Earth, Hand.Full_house, (25, 2);
      Planet.Mars, Hand.Four_of_a_kind, (30, 3);
      Planet.Neptune, Hand.Straight_flush, (40, 4);
      Planet.Neptune, Hand.Royal_flush, (40, 4);
    ]
  in
  List.iter
    (fun (planet, hand, (chips, mult)) ->
       assert (
         HandScoring.score planet hand = [Effect.AddChips chips; Effect.AddMult mult]))
    cases;
  assert (HandScoring.score Planet.Pluto Hand.Pair = []);
  assert (HandScoring.score Planet.Mercury Hand.High_card = []);
  assert (HandScoring.score Planet.Neptune Hand.Flush_five = []);
  let original = List.assoc Hand.Full_house HandScoring.base_hands in
  let upgraded = HandScoring.consume Planet.Earth original in
  assert (upgraded.level = 2);
  assert (upgraded.scoring_effect = Effect.create ~chips:65 ~mult:6);
  let upgraded_again = HandScoring.consume Planet.Earth upgraded in
  assert (upgraded_again.level = 3);
  assert (upgraded_again.scoring_effect = Effect.create ~chips:90 ~mult:8);
  assert (HandScoring.consume Planet.Pluto original = original);
  assert (original.level = 1);
  assert (original.scoring_effect = Effect.create ~chips:40 ~mult:4);
  assert (
    EffectScoring.apply (Effect.MultMult 3) (Effect.create ~chips:40 ~mult:4)
    = Effect.create ~chips:40 ~mult:12)
;;
