open Balatro

(* A run holds a function (its targets), so two runs cannot be compared with [=]. The
   tests compare errors, phases and fields instead. *)

let ok = function
  | Ok x -> x
  | Error e -> failwith (Run.show_error e)
;;

let fails expected = function
  | Ok _ -> false
  | Error e -> e = expected
;;

(* One state per test, shared by every step: a fresh state with the same seed would
   draw the same shop offers again and again. *)
let rng () = Random.State.make [| 42 |]

(* Targets for the tests: [free] wins every blind before any play, [impossible] can
   never be beaten, and [encoded] wins every blind too, but with a value that tells
   which ante and stage asked for it. *)
let free ~ante:_ _ = 0
let impossible ~ante:_ _ = max_int

let stage_number = function
  | Run.Small -> 1
  | Run.Big -> 2
  | Run.Boss -> 3
;;

let encoded ~ante stage = -((10 * ante) + stage_number stage)

let shop_of run =
  match Run.phase run with
  | Run.Shopping { shop; _ } -> shop
  | Run.Playing -> failwith "not shopping"
;;

(* Beats blinds with [free] until a shop shows an offer matching [wanted]; returns its
   position and the run in that shop. *)
let rec shop_with rng wanted run =
  let _, run = ok (Run.cash_out rng run) in
  let offers = List.mapi (fun idx item -> idx, item) (shop_of run).Shop.offers in
  match List.find_opt (fun (_, item) -> wanted item) offers with
  | Some (idx, _) -> idx, run
  | None -> shop_with rng wanted (ok (Run.leave_shop rng run))
;;

let is_joker = function
  | Shop.JokerItem _ -> true
  | Shop.PlanetItem _ -> false
;;

let is_planet item = not (is_joker item)

let test_target () =
  assert (Run.target ~ante:1 Run.Small = 300);
  assert (Run.target ~ante:1 Run.Big = 450);
  assert (Run.target ~ante:1 Run.Boss = 600);
  assert (Run.target ~ante:Run.final_ante Run.Boss = 100_000);
  for ante = 1 to Run.final_ante do
    let small = Run.target ~ante Run.Small in
    let big = Run.target ~ante Run.Big in
    let boss = Run.target ~ante Run.Boss in
    assert (small < big && big < boss);
    if ante > 1 then assert (Run.target ~ante:(ante - 1) Run.Boss < boss)
  done;
  let raises ante =
    match Run.target ~ante Run.Small with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  assert (raises 0);
  assert (raises (Run.final_ante + 1))
;;

let test_create () =
  let run = Run.create (rng ()) Deck.Red in
  assert (Run.ante run = 1);
  assert (Run.equal_stage (Run.stage run) Run.Small);
  assert (Run.money run = Run.starting_money);
  assert (Run.phase run = Run.Playing);
  assert (Run.equal_status (Run.status run) Run.InProgress);
  let blind = Run.blind run in
  assert (Blind.target blind = Run.target ~ante:1 Run.Small);
  assert (Blind.chips blind = 0);
  assert (List.length (Board.hand (Run.board run)) = 8);
  assert (Board.jokers (Run.board run) = []);
  let pair = Poker.Levels.get Poker.Hand.Pair (Run.levels run) in
  assert (pair.level = 1);
  (* [?target] replaces the default targets. *)
  let easy = Run.create ~target:encoded (rng ()) Deck.Red in
  assert (Blind.target (Run.blind easy) = encoded ~ante:1 Run.Small)
;;

let test_play_and_discard () =
  let run = Run.create (rng ()) Deck.Red in
  let _, points, played = ok (Run.play [0] run) in
  assert (points > 0);
  assert (Blind.chips (Run.blind played) = points);
  assert (Blind.plays_left (Run.blind played) = 3);
  (* The run that was played from is left as it was. *)
  assert (Blind.chips (Run.blind run) = 0);
  let discarded = ok (Run.discard [0; 1] run) in
  assert (Blind.discards_left (Run.blind discarded) = 2);
  assert (List.length (Board.hand (Run.board discarded)) = 8);
  let board_error e = Run.BlindError (Blind.BoardError e) in
  assert (fails (board_error Board.NoCards) (Run.play [] run));
  assert (fails (board_error (Board.InvalidIndex 8)) (Run.discard [8] run))
;;

let test_lost () =
  let rec play_all run =
    match Run.play [0] run with
    | Ok (_, _, run) -> play_all run
    | Error _ -> run
  in
  let run = Run.create ~target:impossible (rng ()) Deck.Red |> play_all in
  assert (Blind.plays_left (Run.blind run) = 0);
  assert (Run.equal_status (Run.status run) Run.Lost);
  assert (fails (Run.BlindError Blind.NoPlaysLeft) (Run.play [0] run));
  assert (fails Run.RunOver (Run.cash_out (rng ()) run))
;;

let test_cash_out () =
  let rng = rng () in
  let run = Run.create (Random.State.make [| 42 |]) Deck.Red in
  assert (fails Run.BlindNotOver (Run.cash_out rng run));
  (* Small blind beaten before any play: $3, $4 for the plays left, no interest on $4. *)
  let run = Run.create ~target:free rng Deck.Red in
  let payout, shopping = ok (Run.cash_out rng run) in
  assert (Payout.equal payout (Payout.make ~reward:3 ~plays_left:4 ~money:4));
  assert (Payout.total payout = 7);
  assert (Run.money shopping = 11);
  let shop = shop_of shopping in
  assert (List.length shop.Shop.offers = Shop.offer_count);
  assert (shop.Shop.reroll_cost = Shop.base_reroll_cost);
  (* Only the shop commands work while shopping. *)
  assert (fails Run.InShop (Run.play [0] shopping));
  assert (fails Run.InShop (Run.discard [0] shopping));
  assert (fails Run.InShop (Run.cash_out rng shopping));
  (* Big blind: $4, $4 for the plays and $2 of interest on $11. *)
  let next = ok (Run.leave_shop rng shopping) in
  let payout, shopping = ok (Run.cash_out rng next) in
  assert (payout.Payout.blind = 4);
  assert (payout.Payout.plays = 4);
  assert (payout.Payout.interest = 2);
  assert (Run.money shopping = 21)
;;

(* Every blind of the run, in order, each one with the target of its ante and stage.
   The last boss wins the run, and there is no shop after it. *)
let test_progression () =
  let rng = rng () in
  let blinds =
    List.init Run.final_ante (fun idx -> idx + 1)
    |> List.concat_map (fun ante -> [ante, Run.Small; ante, Run.Big; ante, Run.Boss])
  in
  let rec walk run = function
    | [] -> run
    | (ante, stage) :: rest ->
      assert (Run.ante run = ante);
      assert (Run.equal_stage (Run.stage run) stage);
      assert (Blind.target (Run.blind run) = encoded ~ante stage);
      (match rest with
       | [] -> run
       | _ ->
         assert (Run.equal_status (Run.status run) Run.InProgress);
         let _, shopping = ok (Run.cash_out rng run) in
         walk (ok (Run.leave_shop rng shopping)) rest)
  in
  let last = walk (Run.create ~target:encoded rng Deck.Red) blinds in
  assert (List.length blinds = 24);
  assert (Run.equal_status (Run.status last) Run.Won);
  assert (fails Run.RunOver (Run.cash_out rng last))
;;

(* The next blind starts with every card back in the deck and a full hand. *)
let test_leave_shop_resets_board () =
  let rng = rng () in
  let run = Run.create ~target:(fun ~ante:_ _ -> 1) rng Deck.Red in
  let run = ok (Run.discard [0; 1; 2] run) in
  let _, _, run = ok (Run.play [0] run) in
  assert (Board.discarded (Run.board run) <> []);
  let _, shopping = ok (Run.cash_out rng run) in
  let next = ok (Run.leave_shop rng shopping) in
  let board = Run.board next in
  assert (Board.discarded board = []);
  assert (List.length (Board.hand board) = 8);
  assert (List.length (Deck.cards (Board.deck board)) = 44);
  assert (Blind.plays_left (Run.blind next) = 4);
  assert (Blind.discards_left (Run.blind next) = 3)
;;

let test_buy_and_sell_joker () =
  let rng = rng () in
  let idx, shopping = shop_with rng is_joker (Run.create ~target:free rng Deck.Red) in
  let joker =
    match List.nth (shop_of shopping).Shop.offers idx with
    | Shop.JokerItem joker -> joker
    | Shop.PlanetItem _ -> assert false
  in
  let money = Run.money shopping in
  let bought = ok (Run.buy idx shopping) in
  assert (Run.money bought = money - Joker.cost joker);
  assert (Board.jokers (Run.board bought) = [joker]);
  assert (List.length (shop_of bought).Shop.offers = Shop.offer_count - 1);
  (* The joker goes with the board to the next blind. *)
  let next = ok (Run.leave_shop rng bought) in
  assert (Board.jokers (Run.board next) = [joker]);
  (* Selling gives back [sell_value]; then there is nothing left to sell. *)
  let sold = ok (Run.sell_joker 0 bought) in
  assert (Run.money sold = Run.money bought + Joker.sell_value joker);
  assert (Board.jokers (Run.board sold) = []);
  assert (fails (Run.BoardError (Board.NoJokerAt 0)) (Run.sell_joker 0 sold));
  assert (fails (Run.BoardError (Board.NoJokerAt (-1))) (Run.sell_joker (-1) bought))
;;

let test_buy_planet () =
  let rng = rng () in
  let idx, shopping = shop_with rng is_planet (Run.create ~target:free rng Deck.Red) in
  let planet =
    match List.nth (shop_of shopping).Shop.offers idx with
    | Shop.PlanetItem planet -> planet
    | Shop.JokerItem _ -> assert false
  in
  let level hand run = (Poker.Levels.get hand (Run.levels run)).level in
  let levelled =
    List.filter
      (fun hand -> Consumables.Planet.equal (Poker.Hand.planet hand) planet)
      Poker.Hand.all
  in
  let before = List.map (fun hand -> level hand shopping) levelled in
  let bought = ok (Run.buy idx shopping) in
  assert (Run.money bought = Run.money shopping - 3);
  assert (Board.jokers (Run.board bought) = []);
  (* The planet is used right away, and the new level stays for the next blind. *)
  let next = ok (Run.leave_shop rng bought) in
  List.iter2
    (fun hand old_level ->
       assert (level hand bought = old_level + 1);
       assert (level hand next = old_level + 1))
    levelled
    before
;;

(* $11 after the first blind: rerolls of $5 and $6 leave $0, and then nothing can be
   paid for. *)
let test_reroll_and_money () =
  let rng = rng () in
  let _, shopping = ok (Run.cash_out rng (Run.create ~target:free rng Deck.Red)) in
  assert (Run.money shopping = 11);
  let once = ok (Run.reroll rng shopping) in
  assert (Run.money once = 6);
  assert ((shop_of once).Shop.reroll_cost = 6);
  assert (List.length (shop_of once).Shop.offers = Shop.offer_count);
  let twice = ok (Run.reroll rng once) in
  assert (Run.money twice = 0);
  assert (fails (Run.NotEnoughMoney 7) (Run.reroll rng twice));
  let first = List.hd (shop_of twice).Shop.offers in
  assert (fails (Run.NotEnoughMoney (Shop.price first)) (Run.buy 0 twice));
  assert (fails (Run.ShopError (Shop.NoOfferAt 5)) (Run.buy 5 shopping))
;;

(* A planet used during a blind counts from then on, through the shop and into the
   next blind. *)
let test_use_planet () =
  let rng = rng () in
  let level run = (Poker.Levels.get Poker.Hand.HighCard (Run.levels run)).level in
  let run = Run.create ~target:free rng Deck.Red in
  let run = Run.use_planet Consumables.Planet.Pluto run in
  assert (level run = 2);
  let _, shopping = ok (Run.cash_out rng run) in
  assert (level shopping = 2);
  let next = ok (Run.leave_shop rng shopping) in
  assert (level next = 2)
;;

let () =
  test_target ();
  test_create ();
  test_play_and_discard ();
  test_lost ();
  test_cash_out ();
  test_progression ();
  test_leave_shop_resets_board ();
  test_buy_and_sell_joker ();
  test_buy_planet ();
  test_reroll_and_money ();
  test_use_planet ()
;;
