open Balatro

let levels = Poker.Levels.initial

let fresh_blind ?plays ?discards ~target color =
  let board = Board.create (Random.State.make [| 42 |]) color in
  Blind.create ?plays ?discards ~target board
;;

let play_ok indices blind =
  match Blind.play levels indices blind with
  | Ok result -> result
  | Error e -> failwith (Blind.show_error e)
;;

let discard_ok indices blind =
  match Blind.discard indices blind with
  | Ok blind -> blind
  | Error e -> failwith (Blind.show_error e)
;;

let expect_play_error indices blind expected =
  match Blind.play levels indices blind with
  | Error e -> assert (e = expected)
  | Ok _ -> failwith ("expected " ^ Blind.show_error expected)
;;

let expect_discard_error indices blind expected =
  match Blind.discard indices blind with
  | Error e -> assert (e = expected)
  | Ok _ -> failwith ("expected " ^ Blind.show_error expected)
;;

let hand_size blind = List.length (Board.hand (Blind.board blind))

let test_create color =
  let blind = fresh_blind ~target:300 color in
  assert (Blind.target blind = 300);
  assert (Blind.plays_left blind = 4);
  assert (Blind.discards_left blind = 3);
  assert (Blind.chips blind = 0);
  assert (Blind.status blind = Blind.InProgress);
  (* The round starts with a full hand. *)
  assert (hand_size blind = 8);
  let custom = fresh_blind ~plays:2 ~discards:5 ~target:300 color in
  assert (Blind.plays_left custom = 2);
  assert (Blind.discards_left custom = 5)
;;

let test_play color =
  let blind = fresh_blind ~target:1_000_000 color in
  let _, first_points, after_first = play_ok [0] blind in
  assert (first_points > 0);
  assert (Blind.chips after_first = first_points);
  assert (Blind.plays_left after_first = 3);
  assert (Blind.discards_left after_first = 3);
  (* The hand is refilled after playing. *)
  assert (hand_size after_first = 8);
  let _, second_points, after_second = play_ok [0; 1] after_first in
  assert (Blind.chips after_second = first_points + second_points);
  assert (Blind.plays_left after_second = 2);
  (* The original round remains unchanged. *)
  assert (Blind.chips blind = 0);
  assert (Blind.plays_left blind = 4)
;;

let test_discard color =
  let blind = fresh_blind ~target:300 color in
  let after = discard_ok [0; 1; 2] blind in
  assert (Blind.discards_left after = 2);
  assert (Blind.plays_left after = 4);
  assert (Blind.chips after = 0);
  assert (hand_size after = 8);
  let no_discards = after |> discard_ok [0] |> discard_ok [0] in
  assert (Blind.discards_left no_discards = 0);
  expect_discard_error [0] no_discards Blind.NoDiscardsLeft;
  (* Playing is still allowed without discards. *)
  let _, _, played = play_ok [0] no_discards in
  assert (Blind.plays_left played = 3)
;;

let test_won color =
  let blind = fresh_blind ~target:1 color in
  let _, _, won = play_ok [0] blind in
  assert (Blind.status won = Blind.Won);
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
  assert (Blind.plays_left lost = 0);
  assert (Blind.status lost = Blind.Lost);
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
  List.iter
    (fun color ->
       test_create color;
       test_play color;
       test_discard color;
       test_won color;
       test_lost color;
       test_board_errors color)
    [Deck.Red; Deck.Blue; Deck.Green]
;;
