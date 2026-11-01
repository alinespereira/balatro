open Balatro

let rng () = Random.State.make [| 42 |]
let item = Alcotest.testable Shop.pp_item Shop.equal_item
let shop_error = Alcotest.testable Shop.pp_error ( = )
let run_error = Alcotest.testable Run.pp_error ( = )

let test_price () =
  Alcotest.(check int) "joker" 2 (Shop.price (Shop.JokerItem Joker.Joker));
  Alcotest.(check int) "banner" 5 (Shop.price (Shop.JokerItem Joker.Banner));
  Alcotest.(check int) "pluto" 3 (Shop.price (Shop.PlanetItem Consumables.Planet.Pluto));
  Alcotest.(check int)
    "the star"
    3
    (Shop.price (Shop.TarotItem Consumables.Tarot.TheStar));
  Alcotest.(check int)
    "every joker, planet and implemented tarot is for sale"
    (List.length Joker.all + 12 + List.length Consumables.Tarot.implemented)
    (List.length Shop.all_items)
;;

let test_create () =
  let shop = Shop.create (rng ()) in
  Alcotest.(check int) "offer count" Shop.offer_count (List.length shop.Shop.offers);
  Alcotest.(check int) "reroll cost" Shop.base_reroll_cost shop.Shop.reroll_cost;
  (* No offer shows up twice. *)
  (match shop.Shop.offers with
   | [a; b] -> Alcotest.(check bool) "different offers" false (Shop.equal_item a b)
   | _ -> Alcotest.fail "expected two offers");
  List.iter
    (fun offer ->
       Alcotest.(check bool) "offer is an item" true (List.mem offer Shop.all_items))
    shop.Shop.offers
;;

let test_take () =
  let shop = Shop.create (rng ()) in
  let first = List.nth shop.Shop.offers 0 in
  let second = List.nth shop.Shop.offers 1 in
  (match Shop.take 0 shop with
   | Ok (taken, after) ->
     Alcotest.check item "taken item" first taken;
     Alcotest.(check (list item)) "offers left" [second] after.Shop.offers;
     Alcotest.(check int) "reroll cost" shop.Shop.reroll_cost after.Shop.reroll_cost
   | Error e -> Alcotest.fail (Shop.show_error e));
  let expect_error index =
    match Shop.take index shop with
    | Error e -> Alcotest.check shop_error "error" (Shop.NoOfferAt index) e
    | Ok _ -> Alcotest.fail "expected NoOfferAt"
  in
  expect_error 2;
  expect_error (-1)
;;

let test_reroll () =
  let shop = Shop.create (rng ()) in
  let once = Shop.reroll (rng ()) shop in
  let twice = Shop.reroll (rng ()) once in
  Alcotest.(check int) "offer count" Shop.offer_count (List.length once.Shop.offers);
  Alcotest.(check int) "first reroll" (Shop.base_reroll_cost + 1) once.Shop.reroll_cost;
  Alcotest.(check int) "second reroll" (Shop.base_reroll_cost + 2) twice.Shop.reroll_cost
;;

let joker j = Shop.JokerItem j
let planet p = Shop.PlanetItem p
let secret_planets = Consumables.Planet.[PlanetX; Ceres; Eris]

let check_offered name expected wanted items =
  Alcotest.(check bool) name expected (List.mem wanted items)
;;

let test_available () =
  let levels = Poker.Levels.initial in
  let fresh = Shop.available ~jokers:[] ~levels in
  List.iter (fun j -> check_offered (Joker.show j) true (joker j) fresh) Joker.all;
  check_offered "pluto" true (planet Consumables.Planet.Pluto) fresh;
  check_offered "neptune" true (planet Consumables.Planet.Neptune) fresh;
  (* No secret hand was played yet, so their planets stay hidden. *)
  List.iter
    (fun p -> check_offered (Consumables.Planet.show p) false (planet p) fresh)
    secret_planets;
  Alcotest.(check int)
    "every joker, 9 planets and the implemented tarots"
    (List.length Joker.all + 9 + List.length Consumables.Tarot.implemented)
    (List.length fresh);
  check_offered "the star" true (Shop.TarotItem Consumables.Tarot.TheStar) fresh;
  check_offered
    "the fool is not implemented"
    false
    (Shop.TarotItem Consumables.Tarot.TheFool)
    fresh;
  (* The jokers already owned are not offered again. *)
  let owned = Shop.available ~jokers:Joker.[Banner; Joker] ~levels in
  check_offered "banner is owned" false (joker Joker.Banner) owned;
  check_offered "joker is owned" false (joker Joker.Joker) owned;
  check_offered "cavendish is not" true (joker Joker.Cavendish) owned;
  (* After a flush five, Eris shows up; the other secret planets stay hidden. *)
  let levels = Poker.Levels.record_play Poker.Hand.FlushFive levels in
  let after = Shop.available ~jokers:[] ~levels in
  check_offered "eris" true (planet Consumables.Planet.Eris) after;
  check_offered "planet x" false (planet Consumables.Planet.PlanetX) after;
  check_offered "ceres" false (planet Consumables.Planet.Ceres) after
;;

let test_create_from () =
  let from = [joker Joker.Banner; joker Joker.Joker; planet Consumables.Planet.Pluto] in
  List.init 20 (fun seed -> Random.State.make [| seed |])
  |> List.iter (fun rng ->
    let shop = Shop.create ~from rng in
    let rerolled = Shop.reroll ~from rng shop in
    List.iter
      (fun offer -> check_offered (Shop.show_item offer) true offer from)
      (shop.Shop.offers @ rerolled.Shop.offers));
  (* Fewer items than [offer_count]: the shop offers what there is. *)
  let shop = Shop.create ~from:[joker Joker.Banner] (rng ()) in
  Alcotest.(check (list item)) "only one item" [joker Joker.Banner] shop.Shop.offers
;;

(* A fresh run is playing its first blind: the shop commands are refused, and there is
   nothing to cash out yet. *)
let test_run_outside_shop () =
  let run = Run.create (rng ()) Deck.Red in
  Alcotest.(check bool) "playing" true (Run.phase run = Run.Playing);
  let expect_not_in_shop name = function
    | Error e -> Alcotest.check run_error name Run.NotInShop e
    | Ok _ -> Alcotest.fail (name ^ ": expected NotInShop")
  in
  expect_not_in_shop "buy" (Run.buy 0 run);
  expect_not_in_shop "sell" (Run.sell_joker 0 run);
  expect_not_in_shop "reroll" (Run.reroll (rng ()) run);
  expect_not_in_shop "leave shop" (Run.leave_shop (rng ()) run);
  Alcotest.(check bool) "cash out" true (Result.is_error (Run.cash_out (rng ()) run))
;;

let () =
  Alcotest.run
    "Shop"
    [
      ( "shop",
        [
          Alcotest.test_case "price" `Quick test_price;
          Alcotest.test_case "create" `Quick test_create;
          Alcotest.test_case "take" `Quick test_take;
          Alcotest.test_case "reroll" `Quick test_reroll;
          Alcotest.test_case "available" `Quick test_available;
          Alcotest.test_case "create from" `Quick test_create_from;
        ] );
      "run", [Alcotest.test_case "outside the shop" `Quick test_run_outside_shop];
    ]
;;
