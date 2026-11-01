open Balatro

(* A run holds a function (its targets), so two runs cannot be compared with [=]. The
   tests compare errors, phases and fields instead. *)

let stage = Alcotest.testable Run.pp_stage Run.equal_stage
let status = Alcotest.testable Run.pp_status Run.equal_status
let error = Alcotest.testable Run.pp_error ( = )
let joker = Alcotest.testable Joker.pp Joker.equal
let payout = Alcotest.testable Payout.pp Payout.equal

let ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Run.show_error e)
;;

let expect_error name expected = function
  | Error e -> Alcotest.check error name expected e
  | Ok _ -> Alcotest.fail (name ^ ": expected " ^ Run.show_error expected)
;;

let check_playing name run =
  Alcotest.(check bool) name true (Run.phase run = Run.Playing)
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
  | Run.Playing -> Alcotest.fail "not shopping"
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
  Alcotest.(check int) "ante 1, small" 300 (Run.target ~ante:1 Run.Small);
  Alcotest.(check int) "ante 1, big" 450 (Run.target ~ante:1 Run.Big);
  Alcotest.(check int) "ante 1, boss" 600 (Run.target ~ante:1 Run.Boss);
  Alcotest.(check int) "last boss" 100_000 (Run.target ~ante:Run.final_ante Run.Boss);
  for ante = 1 to Run.final_ante do
    let small = Run.target ~ante Run.Small in
    let big = Run.target ~ante Run.Big in
    let boss = Run.target ~ante Run.Boss in
    let name what = Printf.sprintf "ante %d: %s" ante what in
    Alcotest.(check bool) (name "small < big < boss") true (small < big && big < boss);
    if ante > 1
    then
      Alcotest.(check bool)
        (name "grows from the last boss")
        true
        (Run.target ~ante:(ante - 1) Run.Boss < boss)
  done;
  let raises ante =
    match Run.target ~ante Run.Small with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool) "ante 0 raises" true (raises 0);
  Alcotest.(check bool) "ante after the last raises" true (raises (Run.final_ante + 1))
;;

let test_create () =
  let run = Run.create (rng ()) Deck.Red in
  Alcotest.(check int) "ante" 1 (Run.ante run);
  Alcotest.check stage "stage" Run.Small (Run.stage run);
  Alcotest.(check int) "money" Run.starting_money (Run.money run);
  check_playing "phase" run;
  Alcotest.check status "status" Run.InProgress (Run.status run);
  let blind = Run.blind run in
  Alcotest.(check int) "target" (Run.target ~ante:1 Run.Small) (Blind.target blind);
  Alcotest.(check int) "no chips yet" 0 (Blind.chips blind);
  Alcotest.(check int) "full hand" 8 (List.length (Board.hand (Run.board run)));
  Alcotest.(check (list joker)) "no jokers" [] (Board.jokers (Run.board run));
  let pair = Poker.Levels.get Poker.Hand.Pair (Run.levels run) in
  Alcotest.(check int) "pair level" 1 pair.level;
  (* [?target] replaces the default targets. *)
  let easy = Run.create ~target:encoded (rng ()) Deck.Red in
  Alcotest.(check int)
    "custom target"
    (encoded ~ante:1 Run.Small)
    (Blind.target (Run.blind easy))
;;

let test_play_and_discard () =
  let run = Run.create (rng ()) Deck.Red in
  let _, points, played = ok (Run.play [0] run) in
  Alcotest.(check bool) "a play scores points" true (points > 0);
  Alcotest.(check int) "chips" points (Blind.chips (Run.blind played));
  Alcotest.(check int) "a play is spent" 3 (Blind.plays_left (Run.blind played));
  (* The run that was played from is left as it was. *)
  Alcotest.(check int) "original chips" 0 (Blind.chips (Run.blind run));
  let discarded = ok (Run.discard [0; 1] run) in
  Alcotest.(check int) "a discard is spent" 2 (Blind.discards_left (Run.blind discarded));
  Alcotest.(check int) "hand refilled" 8 (List.length (Board.hand (Run.board discarded)));
  let board_error e = Run.BlindError (Blind.BoardError e) in
  expect_error "play nothing" (board_error Board.NoCards) (Run.play [] run);
  expect_error "bad index" (board_error (Board.InvalidIndex 8)) (Run.discard [8] run)
;;

let test_lost () =
  let rec play_all run =
    match Run.play [0] run with
    | Ok (_, _, run) -> play_all run
    | Error _ -> run
  in
  let run = Run.create ~target:impossible (rng ()) Deck.Red |> play_all in
  Alcotest.(check int) "no plays left" 0 (Blind.plays_left (Run.blind run));
  Alcotest.check status "lost" Run.Lost (Run.status run);
  expect_error "play" (Run.BlindError Blind.NoPlaysLeft) (Run.play [0] run);
  expect_error "cash out" Run.RunOver (Run.cash_out (rng ()) run)
;;

let test_cash_out () =
  let rng = rng () in
  let run = Run.create (Random.State.make [| 42 |]) Deck.Red in
  expect_error "blind not beaten" Run.BlindNotOver (Run.cash_out rng run);
  (* Small blind beaten before any play: $3, $4 for the plays left, no interest on $4. *)
  let run = Run.create ~target:free rng Deck.Red in
  let paid, shopping = ok (Run.cash_out rng run) in
  Alcotest.check
    payout
    "small blind payout"
    (Payout.make ~reward:3 ~plays_left:4 ~money:4)
    paid;
  Alcotest.(check int) "total" 7 (Payout.total paid);
  Alcotest.(check int) "money" 11 (Run.money shopping);
  let shop = shop_of shopping in
  Alcotest.(check int) "offers" Shop.offer_count (List.length shop.Shop.offers);
  Alcotest.(check int) "reroll cost" Shop.base_reroll_cost shop.Shop.reroll_cost;
  (* Only the shop commands work while shopping. *)
  expect_error "play" Run.InShop (Run.play [0] shopping);
  expect_error "discard" Run.InShop (Run.discard [0] shopping);
  expect_error "cash out" Run.InShop (Run.cash_out rng shopping);
  (* Big blind: $4, $4 for the plays and $2 of interest on $11. *)
  let next = ok (Run.leave_shop rng shopping) in
  let paid, shopping = ok (Run.cash_out rng next) in
  Alcotest.(check int) "big blind reward" 4 paid.Payout.blind;
  Alcotest.(check int) "plays left" 4 paid.Payout.plays;
  Alcotest.(check int) "interest on $11" 2 paid.Payout.interest;
  Alcotest.(check int) "money" 21 (Run.money shopping)
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
    | (ante, expected_stage) :: rest ->
      let name what =
        Printf.sprintf "%d %s: %s" ante (Run.show_stage expected_stage) what
      in
      Alcotest.(check int) (name "ante") ante (Run.ante run);
      Alcotest.check stage (name "stage") expected_stage (Run.stage run);
      Alcotest.(check int)
        (name "target")
        (encoded ~ante expected_stage)
        (Blind.target (Run.blind run));
      (match rest with
       | [] -> run
       | _ ->
         Alcotest.check status (name "in progress") Run.InProgress (Run.status run);
         let _, shopping = ok (Run.cash_out rng run) in
         walk (ok (Run.leave_shop rng shopping)) rest)
  in
  let last = walk (Run.create ~target:encoded rng Deck.Red) blinds in
  Alcotest.(check int) "24 blinds" 24 (List.length blinds);
  Alcotest.check status "won" Run.Won (Run.status last);
  expect_error "no shop after the last boss" Run.RunOver (Run.cash_out rng last)
;;

(* The next blind starts with every card back in the deck and a full hand. *)
let test_leave_shop_resets_board () =
  let rng = rng () in
  let run = Run.create ~target:(fun ~ante:_ _ -> 1) rng Deck.Red in
  let run = ok (Run.discard [0; 1; 2] run) in
  let _, _, run = ok (Run.play [0] run) in
  Alcotest.(check bool) "cards were used" true (Board.discarded (Run.board run) <> []);
  let _, shopping = ok (Run.cash_out rng run) in
  let next = ok (Run.leave_shop rng shopping) in
  let board = Run.board next in
  Alcotest.(check int) "nothing discarded" 0 (List.length (Board.discarded board));
  Alcotest.(check int) "full hand" 8 (List.length (Board.hand board));
  Alcotest.(check int) "deck" 44 (List.length (Deck.cards (Board.deck board)));
  Alcotest.(check int) "plays" 4 (Blind.plays_left (Run.blind next));
  Alcotest.(check int) "discards" 3 (Blind.discards_left (Run.blind next))
;;

let test_buy_and_sell_joker () =
  let rng = rng () in
  let idx, shopping = shop_with rng is_joker (Run.create ~target:free rng Deck.Red) in
  let bought_joker =
    match List.nth (shop_of shopping).Shop.offers idx with
    | Shop.JokerItem joker -> joker
    | Shop.PlanetItem _ -> Alcotest.fail "expected a joker"
  in
  let money = Run.money shopping in
  let bought = ok (Run.buy idx shopping) in
  Alcotest.(check int) "pays" (money - Joker.cost bought_joker) (Run.money bought);
  Alcotest.(check (list joker))
    "joker on the board"
    [bought_joker]
    (Board.jokers (Run.board bought));
  Alcotest.(check int)
    "offer taken"
    (Shop.offer_count - 1)
    (List.length (shop_of bought).Shop.offers);
  (* The joker goes with the board to the next blind. *)
  let next = ok (Run.leave_shop rng bought) in
  Alcotest.(check (list joker)) "kept" [bought_joker] (Board.jokers (Run.board next));
  (* Selling gives back [sell_value]; then there is nothing left to sell. *)
  let sold = ok (Run.sell_joker 0 bought) in
  Alcotest.(check int)
    "sell value"
    (Run.money bought + Joker.sell_value bought_joker)
    (Run.money sold);
  Alcotest.(check (list joker)) "joker sold" [] (Board.jokers (Run.board sold));
  expect_error "sell again" (Run.BoardError (Board.NoJokerAt 0)) (Run.sell_joker 0 sold);
  expect_error
    "negative index"
    (Run.BoardError (Board.NoJokerAt (-1)))
    (Run.sell_joker (-1) bought)
;;

let test_buy_planet () =
  let rng = rng () in
  let idx, shopping = shop_with rng is_planet (Run.create ~target:free rng Deck.Red) in
  let planet =
    match List.nth (shop_of shopping).Shop.offers idx with
    | Shop.PlanetItem planet -> planet
    | Shop.JokerItem _ -> Alcotest.fail "expected a planet"
  in
  let level hand run = (Poker.Levels.get hand (Run.levels run)).level in
  let levelled =
    List.filter
      (fun hand -> Consumables.Planet.equal (Poker.Hand.planet hand) planet)
      Poker.Hand.all
  in
  let before = List.map (fun hand -> level hand shopping) levelled in
  let bought = ok (Run.buy idx shopping) in
  Alcotest.(check int) "pays $3" (Run.money shopping - 3) (Run.money bought);
  Alcotest.(check (list joker)) "no joker" [] (Board.jokers (Run.board bought));
  (* The planet is used right away, and the new level stays for the next blind. *)
  let next = ok (Run.leave_shop rng bought) in
  List.iter2
    (fun hand old_level ->
       let name = Poker.Hand.show hand in
       Alcotest.(check int) (name ^ " in the shop") (old_level + 1) (level hand bought);
       Alcotest.(check int) (name ^ " next blind") (old_level + 1) (level hand next))
    levelled
    before
;;

(* $11 after the first blind: rerolls of $5 and $6 leave $0, and then nothing can be
   paid for. *)
let test_reroll_and_money () =
  let rng = rng () in
  let _, shopping = ok (Run.cash_out rng (Run.create ~target:free rng Deck.Red)) in
  Alcotest.(check int) "money" 11 (Run.money shopping);
  let once = ok (Run.reroll rng shopping) in
  Alcotest.(check int) "first reroll costs $5" 6 (Run.money once);
  Alcotest.(check int) "next reroll cost" 6 (shop_of once).Shop.reroll_cost;
  Alcotest.(check int) "offers" Shop.offer_count (List.length (shop_of once).Shop.offers);
  let twice = ok (Run.reroll rng once) in
  Alcotest.(check int) "second reroll costs $6" 0 (Run.money twice);
  expect_error "reroll without money" (Run.NotEnoughMoney 7) (Run.reroll rng twice);
  let first = List.hd (shop_of twice).Shop.offers in
  expect_error
    "buy without money"
    (Run.NotEnoughMoney (Shop.price first))
    (Run.buy 0 twice);
  expect_error "no offer there" (Run.ShopError (Shop.NoOfferAt 5)) (Run.buy 5 shopping)
;;

(* A planet used during a blind counts from then on, through the shop and into the
   next blind. *)
let test_use_planet () =
  let rng = rng () in
  let level run = (Poker.Levels.get Poker.Hand.HighCard (Run.levels run)).level in
  let run = Run.create ~target:free rng Deck.Red in
  let run = Run.use_planet Consumables.Planet.Pluto run in
  Alcotest.(check int) "during the blind" 2 (level run);
  let _, shopping = ok (Run.cash_out rng run) in
  Alcotest.(check int) "in the shop" 2 (level shopping);
  let next = ok (Run.leave_shop rng shopping) in
  Alcotest.(check int) "in the next blind" 2 (level next)
;;

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run
    "Run"
    [
      ( "blinds",
        [
          case "target" test_target;
          case "create" test_create;
          case "play and discard" test_play_and_discard;
          case "lost" test_lost;
          case "progression" test_progression;
        ] );
      ( "shop",
        [
          case "cash out" test_cash_out;
          case "leave shop resets the board" test_leave_shop_resets_board;
          case "buy and sell a joker" test_buy_and_sell_joker;
          case "buy a planet" test_buy_planet;
          case "reroll and money" test_reroll_and_money;
          case "use a planet" test_use_planet;
        ] );
    ]
;;
