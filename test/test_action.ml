open Balatro

let error = Alcotest.testable Run.pp_error ( = )

(* [Card.t] does not derive [show], so its printer is written by hand. *)
let card =
  let pp fmt (c : Card.t) =
    Format.fprintf fmt "%s of %s" (Card.show_rank c.rank) (Card.show_suit c.suit)
  in
  Alcotest.testable pp ( = )
;;

let payout = Alcotest.testable Payout.pp Payout.equal
let rng () = Random.State.make [| 42 |]

(* Any play beats a blind that needs a single point; no play beats [impossible]. *)
let easy ~ante:_ _ = 1
let impossible ~ante:_ _ = max_int

(* The name of an outcome, to compare outcomes without comparing plays. *)
let name = function
  | Action.Played _ -> "Played"
  | Action.BlindWon _ -> "BlindWon"
  | Action.RunWon _ -> "RunWon"
  | Action.RunLost _ -> "RunLost"
  | Action.Done -> "Done"
;;

let apply rng action run =
  match Action.apply rng action run with
  | Ok result -> result
  | Error e -> Alcotest.fail (Run.show_error e)
;;

let expect_error what expected = function
  | Error e -> Alcotest.check error what expected e
  | Ok (outcome, _) -> Alcotest.fail (what ^ ": expected an error, got " ^ name outcome)
;;

let in_shop run =
  match Run.phase run with
  | Run.Shopping _ -> true
  | Run.Playing -> false
;;

let test_played () =
  let run = Run.create ~target:impossible (rng ()) Deck.Red in
  match Action.apply (rng ()) (Action.Play [0]) run with
  | Ok (Action.Played (_, points), next) ->
    let _, expected, _ = Result.get_ok (Run.play [0] run) in
    Alcotest.(check int) "same points as Run.play" expected points;
    Alcotest.(check int) "chips" points (Blind.chips (Run.blind next));
    Alcotest.(check bool) "still playing" false (in_shop next)
  | Ok (outcome, _) -> Alcotest.fail ("expected Played, got " ^ name outcome)
  | Error e -> Alcotest.fail (Run.show_error e)
;;

let test_blind_won () =
  let run = Run.create ~target:easy (rng ()) Deck.Red in
  match Action.apply (rng ()) (Action.Play [0]) run with
  | Ok (Action.BlindWon (_, _, paid), next) ->
    Alcotest.(check bool) "in the shop" true (in_shop next);
    Alcotest.(check int)
      "the payout is in the money"
      (Run.money run + Payout.total paid)
      (Run.money next);
    (* Three plays were left after the winning one. *)
    Alcotest.check
      payout
      "small blind payout"
      (Payout.make ~reward:3 ~plays_left:3 ~money:4)
      paid
  | Ok (outcome, _) -> Alcotest.fail ("expected BlindWon, got " ^ name outcome)
  | Error e -> Alcotest.fail (Run.show_error e)
;;

(* Plays the first card until the run ends, leaving every shop. Gives up after [fuel]
   actions, so a wrong [apply] fails the test instead of looping forever. *)
let rec play_to_the_end ?(fuel = 200) rng run =
  if fuel = 0 then Alcotest.fail "the run never ended";
  let action =
    if in_shop run then
      Action.Leave
    else
      Action.Play [0]
  in
  match apply rng action run with
  | ((Action.RunWon _ | Action.RunLost _) as outcome), _ -> outcome
  | _, next -> play_to_the_end ~fuel:(fuel - 1) rng next
;;

let test_run_won () =
  let rng = rng () in
  let run = Run.create ~target:easy rng Deck.Red in
  Alcotest.(check string) "won" "RunWon" (name (play_to_the_end rng run))
;;

let test_run_lost () =
  let rng = rng () in
  let run = Run.create ~target:impossible rng Deck.Red in
  Alcotest.(check string) "lost" "RunLost" (name (play_to_the_end rng run))
;;

let test_done () =
  let run = Run.create (rng ()) Deck.Red in
  let outcome, discarded = apply (rng ()) (Action.Discard [0; 1]) run in
  Alcotest.(check string) "discard" "Done" (name outcome);
  let expected = Result.get_ok (Run.discard [0; 1] run) in
  Alcotest.(check (list card))
    "same hand as Run.discard"
    (Board.hand (Run.board expected))
    (Board.hand (Run.board discarded));
  (* In the shop: leaving it is Done and starts the next blind. *)
  let rng = rng () in
  let _, shopping = apply rng (Action.Play [0]) (Run.create ~target:easy rng Deck.Red) in
  let outcome, next = apply rng Action.Leave shopping in
  Alcotest.(check string) "leave" "Done" (name outcome);
  Alcotest.(check bool) "playing again" false (in_shop next)
;;

let test_errors () =
  let run = Run.create (rng ()) Deck.Red in
  let board_error e = Run.BlindError (Blind.BoardError e) in
  expect_error
    "play nothing"
    (board_error Board.NoCards)
    (Action.apply (rng ()) (Action.Play []) run);
  expect_error
    "buy while playing"
    Run.NotInShop
    (Action.apply (rng ()) (Action.Buy 0) run);
  expect_error
    "leave while playing"
    Run.NotInShop
    (Action.apply (rng ()) Action.Leave run)
;;

let () =
  Alcotest.run
    "action"
    [
      ( "apply",
        [
          Alcotest.test_case "a play that does not win" `Quick test_played;
          Alcotest.test_case "a play that beats the blind" `Quick test_blind_won;
          Alcotest.test_case "winning the run" `Quick test_run_won;
          Alcotest.test_case "losing the run" `Quick test_run_lost;
          Alcotest.test_case "the other actions" `Quick test_done;
          Alcotest.test_case "errors" `Quick test_errors;
        ] );
    ]
;;
