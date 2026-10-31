open Balatro

let rng () = Random.State.make [| 42 |]

let test_price () =
  assert (Shop.price (Shop.JokerItem Joker.Joker) = 2);
  assert (Shop.price (Shop.JokerItem Joker.Banner) = 5);
  assert (Shop.price (Shop.PlanetItem Consumables.Planet.Pluto) = 3);
  assert (List.length Shop.all_items = List.length Joker.all + 12)
;;

let test_create () =
  let shop = Shop.create (rng ()) in
  assert (List.length shop.Shop.offers = Shop.offer_count);
  assert (shop.Shop.reroll_cost = Shop.base_reroll_cost);
  (* No offer shows up twice. *)
  (match shop.Shop.offers with
   | [a; b] -> assert (not (Shop.equal_item a b))
   | _ -> assert false);
  List.iter (fun offer -> assert (List.mem offer Shop.all_items)) shop.Shop.offers
;;

let test_take () =
  let shop = Shop.create (rng ()) in
  let first = List.nth shop.Shop.offers 0 in
  let second = List.nth shop.Shop.offers 1 in
  (match Shop.take 0 shop with
   | Ok (item, after) ->
     assert (Shop.equal_item item first);
     assert (after.Shop.offers = [second]);
     assert (after.Shop.reroll_cost = shop.Shop.reroll_cost)
   | Error e -> failwith (Shop.show_error e));
  assert (Shop.take 2 shop = Error (Shop.NoOfferAt 2));
  assert (Shop.take (-1) shop = Error (Shop.NoOfferAt (-1)))
;;

let test_reroll () =
  let shop = Shop.create (rng ()) in
  let once = Shop.reroll (rng ()) shop in
  let twice = Shop.reroll (rng ()) once in
  assert (List.length once.Shop.offers = Shop.offer_count);
  assert (once.Shop.reroll_cost = Shop.base_reroll_cost + 1);
  assert (twice.Shop.reroll_cost = Shop.base_reroll_cost + 2)
;;

(* A fresh run is playing its first blind: the shop commands are refused, and there is
   nothing to cash out yet. *)
let test_run_outside_shop () =
  let run = Run.create (rng ()) Deck.Red in
  assert (Run.phase run = Run.Playing);
  assert (Run.buy 0 run = Error Run.NotInShop);
  assert (Run.sell_joker 0 run = Error Run.NotInShop);
  assert (Run.reroll (rng ()) run = Error Run.NotInShop);
  assert (Run.leave_shop (rng ()) run = Error Run.NotInShop);
  assert (Result.is_error (Run.cash_out (rng ()) run))
;;

let () =
  test_price ();
  test_create ();
  test_take ();
  test_reroll ();
  test_run_outside_shop ()
;;
