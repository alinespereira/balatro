open Balatro
open Balatro_text
open Balatro_cli

let rng () = Random.State.make [| 42 |]

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
        (Label.boss_name (Run.boss run))
        (Label.boss_description (Run.boss run));
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
    Printf.sprintf "%s (%s)" (Label.boss_name boss) (Label.boss_description boss)
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
          Alcotest.test_case "hand" `Quick test_hand;
          Alcotest.test_case "play" `Quick test_play;
          Alcotest.test_case "payout" `Quick test_payout;
          Alcotest.test_case "status" `Quick test_status;
          Alcotest.test_case "boss label" `Quick test_status_boss_label;
        ] );
    ]
;;
