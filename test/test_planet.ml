open Balatro

let () =
  let cases =
    [
      (Planet.Pluto, Hand.High_card, (10, 1));
      (Planet.Mercury, Hand.Pair, (15, 1));
      (Planet.Uranus, Hand.Two_pair, (20, 1));
      (Planet.Venus, Hand.Three_of_a_kind, (20, 2));
      (Planet.Saturn, Hand.Straight, (30, 3));
      (Planet.Jupiter, Hand.Flush, (15, 2));
      (Planet.Earth, Hand.Full_house, (25, 2));
      (Planet.Mars, Hand.Four_of_a_kind, (30, 3));
      (Planet.Neptune, Hand.Straight_flush, (40, 4));
      (Planet.Neptune, Hand.Royal_flush, (40, 4));
    ]
  in
  List.iter
    (fun (planet, hand, (chips, mult)) ->
      assert (Planet.score planet hand =
        [ Scoring_effect.AddChips chips; Scoring_effect.AddMult mult ]))
    cases;
  assert (Planet.score Planet.Pluto Hand.Pair = []);
  assert (Planet.score Planet.Mercury Hand.High_card = []);
  assert (Planet.score Planet.Neptune Hand.Flush_five = []);
  let original = List.assoc Hand.Full_house Hand.base_hands in
  let upgraded = Planet.consume Planet.Earth original in
  assert (upgraded.level = 2);
  assert (upgraded.scoring_effect = Scoring_effect.create 65 6);
  let upgraded_again = Planet.consume Planet.Earth upgraded in
  assert (upgraded_again.level = 3);
  assert (upgraded_again.scoring_effect = Scoring_effect.create 90 8);
  assert (Planet.consume Planet.Pluto original = original);
  assert (original.level = 1);
  assert (original.scoring_effect = Scoring_effect.create 40 4);
  assert
    (Scoring_effect.apply (Scoring_effect.MultMult 3)
       (Scoring_effect.create 40 4)
     = Scoring_effect.create 40 12)
