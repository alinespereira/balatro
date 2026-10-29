open Balatro
open Balatro.Consumables

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
      Planet.PlanetX, Poker_hand.FiveOfAKind, (35, 3.);
      Planet.Ceres, Poker_hand.FlushHouse, (40, 4.);
      Planet.Eris, Poker_hand.FlushFive, (50, 3.);
    ]
  in
  List.iter
    (fun (planet, hand, (chips, mult)) ->
       assert (Planet.equal (Planet.of_hand hand) planet);
       assert (Planet.bonus planet = [Score.AddChips chips; Score.AddMult mult]))
    cases;
  let levels = Hand_levels.initial in
  let full_house = Hand_levels.get Poker_hand.FullHouse levels in
  assert (full_house.level = 1);
  assert (Score.equal full_house.score (Score.create ~chips:40 ~mult:4.));
  let levels = Hand_levels.use_planet Planet.Earth levels in
  let full_house = Hand_levels.get Poker_hand.FullHouse levels in
  assert (full_house.level = 2);
  assert (Score.equal full_house.score (Score.create ~chips:65 ~mult:6.));
  let levels = Hand_levels.use_planet Planet.Earth levels in
  let full_house = Hand_levels.get Poker_hand.FullHouse levels in
  assert (full_house.level = 3);
  assert (Score.equal full_house.score (Score.create ~chips:90 ~mult:8.));
  (* A planet only levels up its own hands. *)
  assert ((Hand_levels.get Poker_hand.Pair levels).level = 1);
  (* Neptune levels up straight flush and royal flush together. *)
  let levels = Hand_levels.use_planet Planet.Neptune levels in
  assert ((Hand_levels.get Poker_hand.StraightFlush levels).level = 2);
  assert ((Hand_levels.get Poker_hand.RoyalFlush levels).level = 2);
  (* The initial levels are not changed. *)
  assert ((Hand_levels.get Poker_hand.FullHouse Hand_levels.initial).level = 1);
  assert (
    Score.equal
      (Score.apply (Score.TimesMult 1.5) (Score.create ~chips:40 ~mult:4.))
      (Score.create ~chips:40 ~mult:6.))
;;
