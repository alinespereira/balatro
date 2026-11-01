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
      Printf.sprintf "Meta: %d" (Run.target ~ante:1 Run.Small);
      "";
      "Fichas: 0";
      Printf.sprintf "Jogadas: %d" (Run.blind run).Blind.plays_left;
      Printf.sprintf "Descartes: %d" (Run.blind run).Blind.discards_left;
      Printf.sprintf "Dinheiro: $%d" Run.starting_money;
    ]
    lines
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
          Alcotest.test_case "hand name" `Quick test_hand_name;
          Alcotest.test_case "play" `Quick test_play;
          Alcotest.test_case "payout" `Quick test_payout;
          Alcotest.test_case "status" `Quick test_status;
        ] );
    ]
;;
