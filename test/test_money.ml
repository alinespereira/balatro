open Balatro

let payout ?(reward = 3) ?(plays_left = 0) ?(money = 0) () =
  Payout.make ~reward ~plays_left ~money
;;

let test_parts () =
  let p = payout ~reward:5 ~plays_left:2 ~money:10 () in
  Alcotest.(check int) "blind reward" 5 p.Payout.blind;
  Alcotest.(check int) "$1 per play left" 2 p.Payout.plays;
  Alcotest.(check int) "interest on $10" 2 p.Payout.interest;
  Alcotest.(check int) "total" 9 (Payout.total p)
;;

(* $1 for every $5 held: $4 is not enough, $9 still earns only $1. *)
let test_interest () =
  let interest money = (payout ~money ()).Payout.interest in
  List.iter
    (fun (money, expected) ->
       Alcotest.(check int)
         (Printf.sprintf "interest on $%d" money)
         expected
         (interest money))
    [0, 0; 4, 0; 5, 1; 9, 1; 24, 4; 25, Payout.max_interest; 1_000, Payout.max_interest]
;;

let test_rewards () =
  Alcotest.(check int) "small blind" 3 (Run.reward Run.Small);
  Alcotest.(check int) "big blind" 4 (Run.reward Run.Big);
  Alcotest.(check int) "boss blind" 5 (Run.reward Run.Boss)
;;

let test_starting_money () =
  let run = Run.create (Random.State.make [| 42 |]) Deck.Red in
  Alcotest.(check int) "constant" 4 Run.starting_money;
  Alcotest.(check int) "a new run starts with it" Run.starting_money (Run.money run)
;;

let () =
  Alcotest.run
    "Money"
    [
      ( "payout",
        [
          Alcotest.test_case "parts" `Quick test_parts;
          Alcotest.test_case "interest" `Quick test_interest;
        ] );
      ( "run",
        [
          Alcotest.test_case "rewards" `Quick test_rewards;
          Alcotest.test_case "starting money" `Quick test_starting_money;
        ] );
    ]
;;
