open Balatro
open Balatro_cli

let rng () = Random.State.make [| 42 |]

let test_card () =
  Alcotest.(check string) "king" "K♥" (Render.card (Card.create Card.Hearts Card.King));
  Alcotest.(check string) "ten" "10♠" (Render.card (Card.create Card.Spades Card.Ten))
;;

let test_stage () =
  Alcotest.(check string) "small" "Small Blind" (Render.stage Run.Small);
  Alcotest.(check string) "big" "Big Blind" (Render.stage Run.Big);
  Alcotest.(check string) "boss" "Boss Blind" (Render.stage Run.Boss)
;;

let test_hand_name () =
  Alcotest.(check string) "pair" "Par" (Render.hand_name Poker.Hand.Pair);
  Alcotest.(check string) "two pair" "Dois pares" (Render.hand_name Poker.Hand.TwoPair);
  (* Every hand gets its own name, and none of them is the constructor. *)
  let names = List.map Render.hand_name Poker.Hand.all in
  Alcotest.(check int)
    "names are distinct"
    (List.length names)
    (List.length (List.sort_uniq String.compare names));
  List.iter
    (fun hand ->
       Alcotest.(check bool)
         (Poker.Hand.show hand)
         false
         (Render.hand_name hand = Poker.Hand.show hand))
    Poker.Hand.all
;;

let distinct strings = List.length (List.sort_uniq String.compare strings)

(* Whether [sub] appears somewhere in [s]. *)
let contains ~sub s =
  let n = String.length sub in
  let rec from i = i + n <= String.length s && (String.sub s i n = sub || from (i + 1)) in
  from 0
;;

let test_boss () =
  let eclipses, others =
    List.partition
      (function
        | Boss.Eclipse _ -> true
        | _ -> false)
      Boss.all
  in
  (* Every eclipse has the same name, and every other boss a name of its own. *)
  Alcotest.(check int)
    "one name for the eclipses"
    1
    (distinct (List.map Render.boss_name eclipses));
  Alcotest.(check int)
    "a name for each of the others"
    (List.length others + 1)
    (distinct (List.map Render.boss_name (List.hd eclipses :: others)));
  (* Every boss, each eclipse included, says what it does in its own words. *)
  Alcotest.(check int)
    "a description for each boss"
    (List.length Boss.all)
    (distinct (List.map Render.boss_description Boss.all));
  Alcotest.(check bool)
    "the eclipse names its suit"
    true
    (contains ~sub:"♦" (Render.boss_description (Boss.Eclipse Card.Diamonds)))
;;

let test_play () =
  let cards = [Card.create Card.Hearts Card.King; Card.create Card.Spades Card.King] in
  match Play.make cards with
  | Ok play -> Alcotest.(check string) "pair" "Mão: Par\nPontos: 42" (Render.play play 42)
  | Error _ -> Alcotest.fail "two kings are a valid play"
;;

let test_payout () =
  let payout = Payout.make ~reward:3 ~plays_left:2 ~money:10 in
  Alcotest.(check string)
    "payout"
    "Recompensa do blind: $3\nJogadas restantes: $2\nJuros: $2\nTotal: $7"
    (Render.payout payout)
;;

let test_status () =
  let run = Run.create (rng ()) Deck.Red in
  let lines = String.split_on_char '\n' (Render.status run) in
  Alcotest.(check (list string))
    "first blind"
    [
      "Ante: 1";
      "Rodada: Small Blind";
      Printf.sprintf
        "Chefe do ante: %s (%s)"
        (Render.boss_name (Run.boss run))
        (Render.boss_description (Run.boss run));
      Printf.sprintf "Meta: %d" (Run.target ~ante:1 Run.Small);
      "";
      "Fichas: 0";
      Printf.sprintf "Jogadas: %d" (Run.blind run).Blind.plays_left;
      Printf.sprintf "Descartes: %d" (Run.blind run).Blind.discards_left;
      Printf.sprintf "Dinheiro: $%d" Run.starting_money;
    ]
    lines
;;

(* The boss is only a preview during the Small and Big blinds. *)
let test_status_boss_label () =
  let rng = rng () in
  let ok = function
    | Ok value -> value
    | Error e -> Alcotest.fail (Run.show_error e)
  in
  let next run =
    let _, shopping = ok (Run.cash_out rng run) in
    ok (Run.leave_shop rng shopping)
  in
  let label run = List.nth (String.split_on_char '\n' (Render.status run)) 2 in
  let small = Run.create ~target:(fun ~ante:_ _ -> 0) rng Deck.Red in
  let boss = Run.boss small in
  let described =
    Printf.sprintf "%s (%s)" (Render.boss_name boss) (Render.boss_description boss)
  in
  let big = next small in
  let boss_blind = next big in
  Alcotest.(check string) "small blind" ("Chefe do ante: " ^ described) (label small);
  Alcotest.(check string) "big blind" ("Chefe do ante: " ^ described) (label big);
  Alcotest.(check string) "boss blind" ("Chefe: " ^ described) (label boss_blind)
;;

let test_hand () =
  let cards = [Card.create Card.Hearts Card.King; Card.create Card.Spades Card.Ten] in
  Alcotest.(check (list string))
    "positions over cards"
    ["    1 |    2 "; "-------------"; "   K♥ |  10♠ "]
    (String.split_on_char '\n' (Render.hand cards))
;;

let () =
  Alcotest.run
    "Render"
    [
      ( "render",
        [
          Alcotest.test_case "card" `Quick test_card;
          Alcotest.test_case "hand" `Quick test_hand;
          Alcotest.test_case "stage" `Quick test_stage;
          Alcotest.test_case "boss" `Quick test_boss;
          Alcotest.test_case "hand name" `Quick test_hand_name;
          Alcotest.test_case "play" `Quick test_play;
          Alcotest.test_case "payout" `Quick test_payout;
          Alcotest.test_case "status" `Quick test_status;
          Alcotest.test_case "boss label" `Quick test_status_boss_label;
        ] );
    ]
;;
