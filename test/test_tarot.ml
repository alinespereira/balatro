open Balatro
module Tarot = Consumables.Tarot
module Consumable = Consumables.Consumable

(* [Card.t] does not derive [show], so its printer is written by hand. *)
let card =
  let pp fmt (c : Card.t) =
    Format.fprintf fmt "%s of %s" (Card.show_rank c.rank) (Card.show_suit c.suit)
  in
  Alcotest.testable pp ( = )
;;

let cards = Alcotest.list card
let changed = Alcotest.(list (option card))
let rank = Alcotest.testable Card.pp_rank Card.equal_rank
let tarot = Alcotest.testable Tarot.pp Tarot.equal
let board_error = Alcotest.testable Board.pp_error ( = )
let run_error = Alcotest.testable Run.pp_error ( = )

let consumable =
  Alcotest.testable
    (fun fmt c ->
       Format.pp_print_string
         fmt
         (match c with
          | Consumable.Tarot t -> Tarot.show t
          | Planet p -> Consumables.Planet.show p
          | Spectral s -> Consumables.Spectral.show s))
    ( = )
;;

let board_ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Board.show_error e)
;;

let run_ok = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Run.show_error e)
;;

(* ---- Card ---- *)

let test_next_rank () =
  List.iter
    (fun (from, expected) ->
       Alcotest.check rank (Card.show_rank from) expected (Card.next_rank from))
    Card.[Two, Three; Nine, Ten; Ten, Jack; King, Ace; Ace, Two]
;;

let test_with () =
  let king = Card.create Card.Hearts Card.King in
  Alcotest.check card "with suit" (Card.create Card.Spades Card.King) (Card.with_suit Card.Spades king);
  Alcotest.check card "with rank" (Card.create Card.Hearts Card.Two) (Card.with_rank Card.Two king)
;;

(* ---- Tarot ---- *)

let two_h = Card.create Card.Hearts Card.Two
let king_s = Card.create Card.Spades Card.King
let ace_c = Card.create Card.Clubs Card.Ace

let test_implemented () =
  Alcotest.(check (list tarot))
    "in the order of all"
    (List.filter (fun t -> List.mem t Tarot.implemented) Tarot.all)
    Tarot.implemented
;;

let test_selection () =
  List.iter
    (fun (t, expected) ->
       Alcotest.(check (pair int int)) (Tarot.show t) expected (Tarot.selection t))
    Tarot.
      [
        TheStar, (1, 3);
        TheMoon, (1, 3);
        TheSun, (1, 3);
        TheWorld, (1, 3);
        Strength, (1, 2);
        TheHangedMan, (1, 2);
        Death, (2, 2);
        TheHermit, (0, 0);
        TheFool, (0, 0);
        Judgement, (0, 0);
      ]
;;

let test_change_cards () =
  let same = List.map Option.some in
  let suit s = List.map (fun c -> Some (Card.with_suit s c)) in
  let both = [two_h; king_s] in
  List.iter
    (fun (t, expected) ->
       Alcotest.check changed (Tarot.show t) expected (Tarot.change_cards t both))
    Tarot.
      [
        TheStar, suit Card.Diamonds both;
        TheMoon, suit Card.Clubs both;
        TheSun, suit Card.Hearts both;
        TheWorld, suit Card.Spades both;
        TheHangedMan, [None; None];
        (* The left card becomes a copy of the right one. *)
        Death, [Some king_s; Some king_s];
        TheHermit, same both;
        TheFool, same both;
      ];
  Alcotest.check
    changed
    "strength"
    [Some (Card.create Card.Spades Card.Ace); Some (Card.create Card.Clubs Card.Two)]
    (Tarot.change_cards Tarot.Strength [king_s; ace_c]);
  Alcotest.check
    changed
    "three cards"
    (suit Card.Diamonds [two_h; king_s; ace_c])
    (Tarot.change_cards Tarot.TheStar [two_h; king_s; ace_c])
;;

let test_change_money () =
  List.iter
    (fun (t, money, expected) ->
       Alcotest.(check int)
         (Printf.sprintf "%s with $%d" (Tarot.show t) money)
         expected
         (Tarot.change_money t money))
    Tarot.
      [
        TheHermit, 0, 0;
        TheHermit, 4, 8;
        TheHermit, 20, 40;
        TheHermit, 25, 45;
        TheStar, 7, 7;
        TheFool, 7, 7;
      ]
;;

(* ---- Board ---- *)

let rng () = Random.State.make [| 42 |]
let filled () = Board.refill (Board.create (rng ()) Deck.Red)

let test_consumable_slots () =
  let board = filled () in
  let star = Consumable.Tarot Tarot.TheStar in
  let hermit = Consumable.Tarot Tarot.TheHermit in
  let two = board |> Board.add_consumable star |> board_ok |> Board.add_consumable hermit |> board_ok in
  Alcotest.(check (list consumable)) "left to right" [star; hermit] (Board.consumables two);
  (match Board.add_consumable star two with
   | Error e -> Alcotest.check board_error "no slot" Board.NoConsumableSlot e
   | Ok _ -> Alcotest.fail "expected NoConsumableSlot");
  let taken, one = board_ok (Board.remove_consumable 0 two) in
  Alcotest.check consumable "taken" star taken;
  Alcotest.(check (list consumable)) "left" [hermit] (Board.consumables one);
  List.iter
    (fun idx ->
       match Board.remove_consumable idx one with
       | Error e -> Alcotest.check board_error "nothing there" (Board.NoConsumableAt idx) e
       | Ok _ -> Alcotest.fail "expected NoConsumableAt")
    [1; -1]
;;

let test_use_tarot_changes_in_place () =
  let board = filled () in
  let hand = Board.hand board in
  (* Positions may come in any order. *)
  let starred = board_ok (Board.use_tarot Tarot.TheStar [2; 0] board) in
  let expected =
    List.mapi
      (fun i c ->
         if i = 0 || i = 2 then
           Card.with_suit Card.Diamonds c
         else
           c)
      hand
  in
  Alcotest.check cards "the star" expected (Board.hand starred);
  (* Death gets the cards in hand order: position 1 becomes a copy of position 3. *)
  let died = board_ok (Board.use_tarot Tarot.Death [3; 1] board) in
  let expected = List.mapi (fun i c -> if i = 1 then List.nth hand 3 else c) hand in
  Alcotest.check cards "death" expected (Board.hand died)
;;

let test_use_tarot_destroys () =
  let board = filled () in
  let hand = Board.hand board in
  let hanged = board_ok (Board.use_tarot Tarot.TheHangedMan [4; 1] board) in
  let expected = List.filteri (fun i _ -> i <> 1 && i <> 4) hand in
  Alcotest.check cards "hand" expected (Board.hand hanged);
  Alcotest.check cards "not discarded" [] (Board.discarded hanged);
  (* The destroyed cards never come back. *)
  let reset = Board.reset (rng ()) hanged in
  Alcotest.(check int) "deck after reset" 50 (List.length (Deck.cards (Board.deck reset)))
;;

let test_use_tarot_keeps_changes () =
  let board = filled () in
  let hand = Board.hand board in
  let diamonds cs = List.length (List.filter (fun (c : Card.t) -> c.suit = Card.Diamonds) cs) in
  let starred = board_ok (Board.use_tarot Tarot.TheStar [0; 1; 2] board) in
  let turned = 3 - diamonds (List.filteri (fun i _ -> i < 3) hand) in
  let reset = Board.reset (rng ()) starred in
  Alcotest.(check int)
    "more diamonds in the deck"
    (13 + turned)
    (diamonds (Deck.cards (Board.deck reset)))
;;

let test_use_tarot_errors () =
  let board = filled () in
  let expect name expected tarot indices =
    match Board.use_tarot tarot indices board with
    | Error e -> Alcotest.check board_error name expected e
    | Ok _ -> Alcotest.fail (name ^ ": expected " ^ Board.show_error expected)
  in
  expect "star without cards" (Board.WrongSelection { min = 1; max = 3; got = 0 }) Tarot.TheStar [];
  expect
    "star with four"
    (Board.WrongSelection { min = 1; max = 3; got = 4 })
    Tarot.TheStar
    [0; 1; 2; 3];
  expect "death with one" (Board.WrongSelection { min = 2; max = 2; got = 1 }) Tarot.Death [0];
  expect "hermit with one" (Board.WrongSelection { min = 0; max = 0; got = 1 }) Tarot.TheHermit [0];
  expect "past the hand" (Board.InvalidIndex 8) Tarot.TheStar [8];
  expect "twice" (Board.DuplicateIndex 1) Tarot.TheStar [1; 1];
  let hermit = board_ok (Board.use_tarot Tarot.TheHermit [] board) in
  Alcotest.check cards "hermit leaves the hand" (Board.hand board) (Board.hand hermit)
;;

(* ---- Run ---- *)

let free ~ante:_ _ = 0

let shop_of run =
  match Run.phase run with
  | Run.Shopping { shop; _ } -> shop
  | Run.Playing -> Alcotest.fail "not shopping"
;;

(* Beats blinds with [free] until a shop offers a tarot; returns the tarot, its
   position and the run in that shop. *)
let rec shop_with_tarot rng run =
  let _, run = run_ok (Run.cash_out rng run) in
  let offers = List.mapi (fun idx item -> idx, item) (shop_of run).Shop.offers in
  match
    List.find_map
      (function
        | idx, Shop.TarotItem t -> Some (t, idx)
        | _ -> None)
      offers
  with
  | Some (t, idx) -> t, idx, run
  | None -> shop_with_tarot rng (run_ok (Run.leave_shop rng run))
;;

let test_buy_and_use_in_shop () =
  let rng = rng () in
  let t, idx, shopping = shop_with_tarot rng (Run.create ~target:free rng Deck.Red) in
  let bought = run_ok (Run.buy idx shopping) in
  Alcotest.(check int) "pays $3" (Run.money shopping - 3) (Run.money bought);
  Alcotest.(check (list consumable))
    "in a slot"
    [Consumable.Tarot t]
    (Board.consumables (Run.board bought));
  (* Too many cards: an error, and the tarot is still there. *)
  let min, max = Tarot.selection t in
  (match Run.use_consumable 0 [0; 1; 2; 3] bought with
   | Error e ->
     Alcotest.check
       run_error
       "too many cards"
       (Run.BoardError (Board.WrongSelection { min; max; got = 4 }))
       e
   | Ok _ -> Alcotest.fail "expected WrongSelection");
  let used = run_ok (Run.use_consumable 0 (List.init min Fun.id) bought) in
  Alcotest.(check (list consumable)) "used up" [] (Board.consumables (Run.board used));
  Alcotest.(check int)
    "money"
    (Tarot.change_money t (Run.money bought))
    (Run.money used);
  match Run.use_consumable 0 [] used with
  | Error e -> Alcotest.check run_error "nothing left" (Run.BoardError (Board.NoConsumableAt 0)) e
  | Ok _ -> Alcotest.fail "expected NoConsumableAt"
;;

(* A tarot bought in the shop goes with the board to the next blind and changes the
   hand there. *)
let test_use_in_blind () =
  let rng = rng () in
  let t, idx, shopping = shop_with_tarot rng (Run.create ~target:free rng Deck.Red) in
  let playing = run_ok (Run.leave_shop rng (run_ok (Run.buy idx shopping))) in
  Alcotest.(check bool) "playing" true (Run.phase playing = Run.Playing);
  let _, max = Tarot.selection t in
  let indices = List.init max Fun.id in
  let board = Run.board playing in
  let _, without_tarot = board_ok (Board.remove_consumable 0 board) in
  let expected = board_ok (Board.use_tarot t indices without_tarot) in
  let used = run_ok (Run.use_consumable 0 indices playing) in
  Alcotest.check cards "hand" (Board.hand expected) (Board.hand (Run.board used));
  Alcotest.check cards "blind's board" (Board.hand expected) (Board.hand (Blind.board (Run.blind used)))
;;

let () =
  let case name test = Alcotest.test_case name `Quick test in
  Alcotest.run
    "Tarot"
    [
      "card", [case "next rank" test_next_rank; case "with suit and rank" test_with];
      ( "tarot",
        [
          case "implemented" test_implemented;
          case "selection" test_selection;
          case "change cards" test_change_cards;
          case "change money" test_change_money;
        ] );
      ( "board",
        [
          case "consumable slots" test_consumable_slots;
          case "changes in place" test_use_tarot_changes_in_place;
          case "destroys" test_use_tarot_destroys;
          case "changes stay in the deck" test_use_tarot_keeps_changes;
          case "errors" test_use_tarot_errors;
        ] );
      ( "run",
        [
          case "buy and use in the shop" test_buy_and_use_in_shop;
          case "use in a blind" test_use_in_blind;
        ] );
    ]
;;
