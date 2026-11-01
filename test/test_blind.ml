open Balatro

let levels = Poker.Levels.initial

let fresh_board ?(jokers = []) color =
  let board = Board.create (Random.State.make [| 42 |]) color in
  List.fold_left
    (fun board joker ->
       match Board.add_joker joker board with
       | Ok board -> board
       | Error e -> Alcotest.fail (Board.show_error e))
    board
    jokers
;;

let fresh_blind ?boss ?jokers ?plays ?discards ~target color =
  Blind.create ?boss ?plays ?discards ~target (fresh_board ?jokers color)
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

(* Bosses. *)

let boss = Alcotest.testable Boss.pp Boss.equal
let joker = Alcotest.testable Joker.pp Joker.equal
let jokers_of blind = Board.jokers (Blind.board blind)

(* Points of a high card that no card scores for: the level alone. *)
let bare_high_card =
  Score.total (Poker.Levels.get Poker.Hand.HighCard levels).Poker.Levels.score
;;

let first_card blind = List.hd (Board.hand (Blind.board blind))

let suit_joker = function
  | Card.Diamonds -> Joker.GreedyJoker
  | Card.Hearts -> Joker.LustyJoker
  | Card.Spades -> Joker.WrathfulJoker
  | Card.Clubs -> Joker.GluttonousJoker
;;

let test_no_boss () =
  let blind = fresh_blind ~target:300 Deck.Red in
  Alcotest.(check (option boss)) "no boss" None (Blind.boss blind)
;;

let test_boss_limits () =
  let drought = fresh_blind ~boss:Boss.Drought ~target:300 Deck.Red in
  Alcotest.(check (option boss)) "the boss" (Some Boss.Drought) (Blind.boss drought);
  Alcotest.(check int) "drought: no discards" 0 (Blind.discards_left drought);
  expect_discard_error [0] drought Blind.NoDiscardsLeft;
  let hourglass = fresh_blind ~boss:Boss.Hourglass ~target:300 Deck.Red in
  Alcotest.(check int) "hourglass: 3 plays" 3 (Blind.plays_left hourglass);
  let giant = fresh_blind ~boss:Boss.Giant ~target:300 Deck.Red in
  Alcotest.(check int) "giant: target 450" 450 (Blind.target giant)
;;

(* Under an eclipse of its own suit, a lone card scores nothing, and the joker of that
   suit does not see it either. *)
let test_eclipse () =
  let suit = (first_card (fresh_blind ~target:1_000 Deck.Red)).Card.suit in
  let blind =
    fresh_blind ~boss:(Boss.Eclipse suit) ~jokers:[suit_joker suit] ~target:1_000 Deck.Red
  in
  let _, points, _ = play_ok [0] blind in
  Alcotest.(check int) "only the hand's level" bare_high_card points
;;

let test_muzzle () =
  let plain = fresh_blind ~target:1_000 Deck.Red in
  let muzzled =
    fresh_blind
      ~boss:Boss.Muzzle
      ~jokers:[Joker.Joker; Joker.Cavendish]
      ~target:1_000
      Deck.Red
  in
  let _, plain_points, _ = play_ok [0; 1] plain in
  let _, muzzled_points, _ = play_ok [0; 1] muzzled in
  Alcotest.(check int) "as if there were no jokers" plain_points muzzled_points
;;

(* Under the muzzle, the jokers do not react either: the Green Joker does not grow. *)
let test_muzzle_no_reaction () =
  let muzzled =
    fresh_blind
      ~boss:Boss.Muzzle
      ~jokers:Joker.[GreenJoker { mult = 0 }]
      ~target:1_000
      Deck.Red
  in
  let _, _, played = play_ok [0] muzzled in
  Alcotest.(check (list joker))
    "green joker stays at 0"
    Joker.[GreenJoker { mult = 0 }]
    (jokers_of played)
;;

(* The jokers react after every play and discard, and the board keeps the new state:
   the Green Joker grows and shrinks, and the Ice Cream melts on its last 5 chips. *)
let test_jokers_react color =
  let blind =
    fresh_blind
      ~jokers:Joker.[GreenJoker { mult = 0 }; IceCream { chips = 5 }]
      ~target:100_000
      color
  in
  let _, _, played = play_ok [0] blind in
  Alcotest.(check (list joker))
    "after a play"
    Joker.[GreenJoker { mult = 1 }]
    (jokers_of played);
  let discarded = discard_ok [0] played in
  Alcotest.(check (list joker))
    "after a discard"
    Joker.[GreenJoker { mult = 0 }]
    (jokers_of discarded)
;;

(* A Green Joker at +2 adds 2 to the mult of the play. *)
let test_grown_joker_scores color =
  let plain = fresh_blind ~target:100_000 color in
  let grown = fresh_blind ~jokers:Joker.[GreenJoker { mult = 2 }] ~target:100_000 color in
  let play, plain_points, _ = play_ok [0] plain in
  let _, grown_points, _ = play_ok [0] grown in
  let { Score.chips; Score.mult } = Play.score levels play in
  Alcotest.(check int)
    "without the joker"
    (Score.total (Score.create ~chips ~mult))
    plain_points;
  Alcotest.(check int)
    "two more mult"
    (Score.total (Score.create ~chips ~mult:(mult +. 2.)))
    grown_points
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
        "jokers react", test_jokers_react;
        "grown joker scores", test_grown_joker_scores;
      ]
  in
  let colors = [Deck.Red; Deck.Blue; Deck.Green] in
  let bosses =
    List.map
      (fun (name, test) -> Alcotest.test_case name `Quick test)
      [
        "no boss", test_no_boss;
        "limits", test_boss_limits;
        "eclipse", test_eclipse;
        "muzzle", test_muzzle;
        "muzzle: no reaction", test_muzzle_no_reaction;
      ]
  in
  Alcotest.run
    "Blind"
    (List.map (fun color -> color_name color, cases color) colors @ ["boss", bosses])
;;
