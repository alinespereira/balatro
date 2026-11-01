open Balatro

let levels = Poker.Levels.initial

let fresh_blind ?plays ?discards ~target color =
  let board = Board.create (Random.State.make [| 42 |]) color in
  Blind.create ?plays ?discards ~target board
;;

(* Testables tell Alcotest how to compare and print values of our own types. [status]
   derives [eq]; [error] does not, so it uses structural equality. *)
let status = Alcotest.testable Blind.pp_status Blind.equal_status
let error = Alcotest.testable Blind.pp_error ( = )

let play_ok indices blind =
  match Blind.play levels indices blind with
  | Ok result -> result
  | Error e -> Alcotest.fail (Blind.show_error e)
;;

let discard_ok indices blind =
  match Blind.discard indices blind with
  | Ok blind -> blind
  | Error e -> Alcotest.fail (Blind.show_error e)
;;

let expect_play_error indices blind expected =
  match Blind.play levels indices blind with
  | Error e -> Alcotest.check error "play error" expected e
  | Ok _ -> Alcotest.fail ("expected " ^ Blind.show_error expected)
;;

let expect_discard_error indices blind expected =
  match Blind.discard indices blind with
  | Error e -> Alcotest.check error "discard error" expected e
  | Ok _ -> Alcotest.fail ("expected " ^ Blind.show_error expected)
;;

let hand_size blind = List.length (Board.hand (Blind.board blind))

let test_create color =
  let blind = fresh_blind ~target:300 color in
  Alcotest.(check int) "target" 300 (Blind.target blind);
  Alcotest.(check int) "4 plays by default" 4 (Blind.plays_left blind);
  Alcotest.(check int) "3 discards by default" 3 (Blind.discards_left blind);
  Alcotest.(check int) "no chips yet" 0 (Blind.chips blind);
  Alcotest.check status "in progress" Blind.InProgress (Blind.status blind);
  (* The round starts with a full hand. *)
  Alcotest.(check int) "full hand" 8 (hand_size blind);
  let custom = fresh_blind ~plays:2 ~discards:5 ~target:300 color in
  Alcotest.(check int) "custom plays" 2 (Blind.plays_left custom);
  Alcotest.(check int) "custom discards" 5 (Blind.discards_left custom)
;;

let test_play color =
  let blind = fresh_blind ~target:1_000_000 color in
  let _, first_points, after_first = play_ok [0] blind in
  Alcotest.(check bool) "a play scores points" true (first_points > 0);
  Alcotest.(check int) "chips after one play" first_points (Blind.chips after_first);
  Alcotest.(check int) "a play is spent" 3 (Blind.plays_left after_first);
  Alcotest.(check int) "discards are kept" 3 (Blind.discards_left after_first);
  (* The hand is refilled after playing. *)
  Alcotest.(check int) "hand refilled" 8 (hand_size after_first);
  let _, second_points, after_second = play_ok [0; 1] after_first in
  Alcotest.(check int)
    "chips add up"
    (first_points + second_points)
    (Blind.chips after_second);
  Alcotest.(check int) "two plays spent" 2 (Blind.plays_left after_second);
  (* The original round remains unchanged. *)
  Alcotest.(check int) "original chips" 0 (Blind.chips blind);
  Alcotest.(check int) "original plays" 4 (Blind.plays_left blind)
;;

let test_discard color =
  let blind = fresh_blind ~target:300 color in
  let after = discard_ok [0; 1; 2] blind in
  Alcotest.(check int) "a discard is spent" 2 (Blind.discards_left after);
  Alcotest.(check int) "plays are kept" 4 (Blind.plays_left after);
  Alcotest.(check int) "discarding scores nothing" 0 (Blind.chips after);
  Alcotest.(check int) "hand refilled" 8 (hand_size after);
  let no_discards = after |> discard_ok [0] |> discard_ok [0] in
  Alcotest.(check int) "no discards left" 0 (Blind.discards_left no_discards);
  expect_discard_error [0] no_discards Blind.NoDiscardsLeft;
  (* Playing is still allowed without discards. *)
  let _, _, played = play_ok [0] no_discards in
  Alcotest.(check int) "can still play" 3 (Blind.plays_left played)
;;

let test_won color =
  let blind = fresh_blind ~target:1 color in
  let _, _, won = play_ok [0] blind in
  Alcotest.check status "won" Blind.Won (Blind.status won);
  expect_play_error [0] won Blind.RoundOver;
  expect_discard_error [0] won Blind.RoundOver
;;

let test_lost color =
  let blind = fresh_blind ~target:1_000_000 color in
  let play blind =
    let _, _, blind = play_ok [0] blind in
    blind
  in
  let lost = blind |> play |> play |> play |> play in
  Alcotest.(check int) "no plays left" 0 (Blind.plays_left lost);
  Alcotest.check status "lost" Blind.Lost (Blind.status lost);
  expect_play_error [0] lost Blind.NoPlaysLeft;
  expect_discard_error [0] lost Blind.RoundOver
;;

(* An invalid selection is reported and does not spend a play or a discard. *)
let test_board_errors color =
  let blind = fresh_blind ~target:300 color in
  expect_play_error [8] blind (Blind.BoardError (Board.InvalidIndex 8));
  expect_discard_error [] blind (Blind.BoardError Board.NoCards);
  expect_play_error [0; 0] blind (Blind.BoardError (Board.DuplicateIndex 0))
;;

let () =
  let color_name = function
    | Deck.Red -> "red"
    | Deck.Blue -> "blue"
    | Deck.Green -> "green"
  in
  let cases color =
    List.map
      (fun (name, test) -> Alcotest.test_case name `Quick (fun () -> test color))
      [
        "create", test_create;
        "play", test_play;
        "discard", test_discard;
        "won", test_won;
        "lost", test_lost;
        "board errors", test_board_errors;
      ]
  in
  let colors = [Deck.Red; Deck.Blue; Deck.Green] in
  Alcotest.run "Blind" (List.map (fun color -> color_name color, cases color) colors)
;;
