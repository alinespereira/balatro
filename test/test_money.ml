open Balatro

let payout ?(reward = 3) ?(plays_left = 0) ?(money = 0) () =
  Payout.make ~reward ~plays_left ~money
;;

let test_parts () =
  let p = payout ~reward:5 ~plays_left:2 ~money:10 () in
  assert (p.Payout.blind = 5);
  assert (p.Payout.plays = 2);
  assert (p.Payout.interest = 2);
  assert (Payout.total p = 9)
;;

(* $1 for every $5 held: $4 is not enough, $9 still earns only $1. *)
let test_interest () =
  let interest money = (payout ~money ()).Payout.interest in
  assert (interest 0 = 0);
  assert (interest 4 = 0);
  assert (interest 5 = 1);
  assert (interest 9 = 1);
  assert (interest 24 = 4);
  assert (interest 25 = Payout.max_interest);
  assert (interest 1_000 = Payout.max_interest)
;;

let test_rewards () =
  assert (Run.reward Run.Small = 3);
  assert (Run.reward Run.Big = 4);
  assert (Run.reward Run.Boss = 5)
;;

let test_starting_money () =
  let run = Run.create (Random.State.make [| 42 |]) Deck.Red in
  assert (Run.money run = Run.starting_money);
  assert (Run.starting_money = 4)
;;

let () =
  test_parts ();
  test_interest ();
  test_rewards ();
  test_starting_money ()
