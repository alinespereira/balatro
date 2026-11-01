open Balatro
open Balatro.Consumables

let planet = Alcotest.testable Planet.pp Planet.equal
let score = Alcotest.testable Score.pp Score.equal
let change = Alcotest.testable Score.pp_change Score.equal_change

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
;;

(* One test case per row of the table, so a failure names the hand. *)
let bonus_cases =
  List.map
    (fun (expected, hand, (chips, mult)) ->
       Alcotest.test_case (Poker.Hand.show hand) `Quick (fun () ->
         Alcotest.check planet "planet of the hand" expected (Poker.Hand.planet hand);
         Alcotest.(check (list change))
           "bonus"
           [Score.AddChips chips; Score.AddMult mult]
           (Planet.bonus expected)))
    cases
;;

let level hand levels = (Poker.Levels.get hand levels).level

let test_level_up () =
  let check_full_house levels ~level:expected ~chips ~mult =
    let full_house = Poker.Levels.get Poker.Hand.FullHouse levels in
    Alcotest.(check int) "level" expected full_house.level;
    Alcotest.check score "score" (Score.create ~chips ~mult) full_house.score
  in
  let levels = Poker.Levels.initial in
  check_full_house levels ~level:1 ~chips:40 ~mult:4.;
  let levels = Poker.Levels.use_planet Planet.Earth levels in
  check_full_house levels ~level:2 ~chips:65 ~mult:6.;
  let levels = Poker.Levels.use_planet Planet.Earth levels in
  check_full_house levels ~level:3 ~chips:90 ~mult:8.;
  (* A planet only levels up its own hands. *)
  Alcotest.(check int) "pair is untouched" 1 (level Poker.Hand.Pair levels);
  (* The initial levels are not changed. *)
  Alcotest.(check int)
    "initial levels are untouched"
    1
    (level Poker.Hand.FullHouse Poker.Levels.initial)
;;

(* Neptune levels up straight flush and royal flush together. *)
let test_neptune () =
  let levels = Poker.Levels.use_planet Planet.Neptune Poker.Levels.initial in
  Alcotest.(check int) "straight flush" 2 (level Poker.Hand.StraightFlush levels);
  Alcotest.(check int) "royal flush" 2 (level Poker.Hand.RoyalFlush levels)
;;

let played hand levels = (Poker.Levels.get hand levels).played

let test_record_play () =
  List.iter
    (fun hand ->
       Alcotest.(check int)
         (Poker.Hand.show hand ^ " never played")
         0
         (played hand Poker.Levels.initial))
    Poker.Hand.all;
  let levels =
    Poker.Levels.initial
    |> Poker.Levels.record_play Poker.Hand.Pair
    |> Poker.Levels.record_play Poker.Hand.Pair
  in
  Alcotest.(check int) "pair played twice" 2 (played Poker.Hand.Pair levels);
  Alcotest.(check int) "flush never played" 0 (played Poker.Hand.Flush levels);
  (* Playing a hand does not level it up. *)
  Alcotest.(check int) "pair still at level 1" 1 (level Poker.Hand.Pair levels);
  (* And a planet keeps the count. *)
  let levels = Poker.Levels.use_planet Planet.Mercury levels in
  Alcotest.(check int) "count after a planet" 2 (played Poker.Hand.Pair levels)
;;

let test_times_mult () =
  Alcotest.check
    score
    "x1.5 mult"
    (Score.create ~chips:40 ~mult:6.)
    (Score.apply (Score.TimesMult 1.5) (Score.create ~chips:40 ~mult:4.))
;;

let () =
  Alcotest.run
    "Planet"
    [
      "bonus", bonus_cases;
      ( "levels",
        [
          Alcotest.test_case "level up" `Quick test_level_up;
          Alcotest.test_case "neptune" `Quick test_neptune;
          Alcotest.test_case "record play" `Quick test_record_play;
          Alcotest.test_case "times mult" `Quick test_times_mult;
        ] );
    ]
;;
