open Balatro
open Balatro.Consumables

let () =
  let cases =
    [
      Planet.Pluto, Poker.Hand.HighCard, (10, 1.);
      Planet.Mercury, Poker.Hand.Pair, (15, 1.);
      Planet.Uranus, Poker.Hand.TwoPair, (20, 1.);
      Planet.Venus, Poker.Hand.ThreeOfAKind, (20, 2.);
      Planet.Saturn, Poker.Hand.Straight, (30, 3.);
      Planet.Jupiter, Poker.Hand.Flush, (15, 2.);
      Planet.Earth, Poker.Hand.FullHouse, (25, 2.);
      Planet.Mars, Poker.Hand.FourOfAKind, (30, 3.);
      Planet.Neptune, Poker.Hand.StraightFlush, (40, 4.);
      Planet.Neptune, Poker.Hand.RoyalFlush, (40, 4.);
      Planet.PlanetX, Poker.Hand.FiveOfAKind, (35, 3.);
      Planet.Ceres, Poker.Hand.FlushHouse, (40, 4.);
      Planet.Eris, Poker.Hand.FlushFive, (50, 3.);
    ]
  in
  List.iter
    (fun (planet, hand, (chips, mult)) ->
       assert (Planet.equal (Poker.Hand.planet hand) planet);
       assert (Planet.bonus planet = [Score.AddChips chips; Score.AddMult mult]))
    cases;
  let levels = Poker.Levels.initial in
  let full_house = Poker.Levels.get Poker.Hand.FullHouse levels in
  assert (full_house.level = 1);
  assert (Score.equal full_house.score (Score.create ~chips:40 ~mult:4.));
  let levels = Poker.Levels.use_planet Planet.Earth levels in
  let full_house = Poker.Levels.get Poker.Hand.FullHouse levels in
  assert (full_house.level = 2);
  assert (Score.equal full_house.score (Score.create ~chips:65 ~mult:6.));
  let levels = Poker.Levels.use_planet Planet.Earth levels in
  let full_house = Poker.Levels.get Poker.Hand.FullHouse levels in
  assert (full_house.level = 3);
  assert (Score.equal full_house.score (Score.create ~chips:90 ~mult:8.));
  (* A planet only levels up its own hands. *)
  assert ((Poker.Levels.get Poker.Hand.Pair levels).level = 1);
  (* Neptune levels up straight flush and royal flush together. *)
  let levels = Poker.Levels.use_planet Planet.Neptune levels in
  assert ((Poker.Levels.get Poker.Hand.StraightFlush levels).level = 2);
  assert ((Poker.Levels.get Poker.Hand.RoyalFlush levels).level = 2);
  (* The initial levels are not changed. *)
  assert ((Poker.Levels.get Poker.Hand.FullHouse Poker.Levels.initial).level = 1);
  assert (
    Score.equal
      (Score.apply (Score.TimesMult 1.5) (Score.create ~chips:40 ~mult:4.))
      (Score.create ~chips:40 ~mult:6.))
;;
