open Balatro

let limits = Alcotest.testable Boss.pp_limits Boss.equal_limits
let default = { Boss.plays = 4; Boss.discards = 3; Boss.target = 600 }
let card suit rank = Card.create suit rank

let test_all () =
  (* Five bosses, plus one eclipse for each of the four suits. *)
  Alcotest.(check int) "nine bosses" 9 (List.length Boss.all);
  List.iter
    (fun suit ->
       Alcotest.(check bool)
         (Card.show_suit suit)
         true
         (List.mem (Boss.Eclipse suit) Boss.all))
    Card.all_of_suit
;;

let test_random () =
  let rng = Random.State.make [| 42 |] in
  let drawn = List.init 200 (fun _ -> Boss.random rng) in
  List.iter
    (fun b -> Alcotest.(check bool) (Boss.show b) true (List.mem b Boss.all))
    drawn;
  (* 200 draws out of 9 bosses see more than one of them. *)
  Alcotest.(check bool)
    "not always the same"
    true
    (List.exists (fun b -> not (Boss.equal b (List.hd drawn))) drawn)
;;

let boss = Alcotest.testable Boss.pp Boss.equal

let test_random_avoid () =
  let rng = Random.State.make [| 42 |] in
  let avoid = Boss.[Fog; Muzzle; Drought] in
  List.init 200 (fun _ -> Boss.random ~avoid rng)
  |> List.iter (fun b ->
    Alcotest.(check bool) (Boss.show b ^ " is not avoided") false (List.mem b avoid));
  (* Only the giant is left, so it is always the one drawn. *)
  let all_but_giant = List.filter (fun b -> not (Boss.equal b Boss.Giant)) Boss.all in
  List.init 20 (fun _ -> Boss.random ~avoid:all_but_giant rng)
  |> List.iter (Alcotest.check boss "the only one left" Boss.Giant);
  (* Every boss avoided: it draws from all of them again. *)
  List.init 20 (fun _ -> Boss.random ~avoid:Boss.all rng)
  |> List.iter (fun b -> Alcotest.(check bool) (Boss.show b) true (List.mem b Boss.all))
;;

let test_limits () =
  let check name expected b =
    Alcotest.check limits name expected (Boss.limits b default)
  in
  check "drought: no discards" { default with Boss.discards = 0 } Boss.Drought;
  check "hourglass: one play fewer" { default with Boss.plays = 3 } Boss.Hourglass;
  check "giant: 50% more" { default with Boss.target = 900 } Boss.Giant;
  (* The others keep the limits as they are. *)
  check "eclipse" default (Boss.Eclipse Card.Hearts);
  check "fog" default Boss.Fog;
  check "muzzle" default Boss.Muzzle;
  (* Never less than one play, and the target rounds down. *)
  Alcotest.check
    limits
    "hourglass with one play"
    { Boss.plays = 1; Boss.discards = 3; Boss.target = 5 }
    (Boss.limits Boss.Hourglass { Boss.plays = 1; Boss.discards = 3; Boss.target = 5 });
  Alcotest.check
    limits
    "giant rounds down"
    { Boss.plays = 4; Boss.discards = 3; Boss.target = 7 }
    (Boss.limits Boss.Giant { Boss.plays = 4; Boss.discards = 3; Boss.target = 5 })
;;

let test_debuffs () =
  let check name expected b c = Alcotest.(check bool) name expected (Boss.debuffs b c) in
  let king_of_hearts = card Card.Hearts Card.King in
  let ace_of_hearts = card Card.Hearts Card.Ace in
  let two_of_spades = card Card.Spades Card.Two in
  check "eclipse ♥: K♥" true (Boss.Eclipse Card.Hearts) king_of_hearts;
  check "eclipse ♥: 2♠" false (Boss.Eclipse Card.Hearts) two_of_spades;
  check "eclipse ♠: 2♠" true (Boss.Eclipse Card.Spades) two_of_spades;
  check "fog: K♥" true Boss.Fog king_of_hearts;
  check "fog: J" true Boss.Fog (card Card.Clubs Card.Jack);
  check "fog: Q" true Boss.Fog (card Card.Diamonds Card.Queen);
  (* An ace is not a face card. *)
  check "fog: A♥" false Boss.Fog ace_of_hearts;
  (* The bosses without a debuff let every card score. *)
  List.iter
    (fun b ->
       List.iter
         (fun c -> check (Boss.show b) false b c)
         [king_of_hearts; ace_of_hearts; two_of_spades])
    [Boss.Drought; Boss.Hourglass; Boss.Giant; Boss.Muzzle]
;;

let test_disables_jokers () =
  List.iter
    (fun b ->
       Alcotest.(check bool)
         (Boss.show b)
         (Boss.equal b Boss.Muzzle)
         (Boss.disables_jokers b))
    Boss.all
;;

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run
    "Boss"
    [
      ( "boss",
        [
          case "all" test_all;
          case "random" test_random;
          case "random avoids" test_random_avoid;
          case "limits" test_limits;
          case "debuffs" test_debuffs;
          case "disables jokers" test_disables_jokers;
        ] );
    ]
;;
