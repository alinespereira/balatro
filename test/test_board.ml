open Balatro

let fresh_board color = Board.create (Random.State.make [| 42 |]) color

let test_refill color =
  let board = fresh_board color in
  let original_cards = Deck.cards (Board.deck board) in
  assert (List.length original_cards = 52);
  assert (Board.hand board = []);
  let filled = Board.refill board in
  let drawn = Board.hand filled in
  let remaining = Deck.cards (Board.deck filled) in
  assert (List.length drawn = 8);
  assert (List.length remaining = 44);
  (* Every original card is in exactly one place: the hand or the deck. *)
  List.iter
    (fun card ->
       let occurrences = List.filter (Card.equal card) (drawn @ remaining) in
       assert (List.length occurrences = 1))
    original_cards;
  (* Filling an already full hand must not consume more cards. *)
  let refilled = Board.refill filled in
  assert (Board.hand refilled = drawn);
  assert (Deck.cards (Board.deck refilled) = remaining);
  (* The original board remains unchanged. *)
  assert (Board.hand board = []);
  assert (Deck.cards (Board.deck board) = original_cards)
;;

let test_play_and_discard color =
  let board = fresh_board color |> Board.refill in
  let hand = Board.hand board in
  (match Board.discard [0; 2] board with
   | Ok after ->
     assert (List.length (Board.hand after) = 6);
     assert (Board.discarded after = [List.nth hand 0; List.nth hand 2]);
     let refilled = Board.refill after in
     assert (List.length (Board.hand refilled) = 8);
     assert (List.length (Deck.cards (Board.deck refilled)) = 42)
   | Error e -> failwith (Board.show_error e));
  match Board.play [1] board with
  | Ok (play, after) ->
    assert (Play.scoring_cards play = [List.nth hand 1]);
    assert (List.length (Board.hand after) = 7)
  | Error e -> failwith (Board.show_error e)
;;

let test_invalid_selections color =
  let board = fresh_board color |> Board.refill in
  let expect_error indices expected =
    assert (Board.discard indices board = Error expected);
    match Board.play indices board with
    | Error e -> assert (e = expected)
    | Ok _ -> assert false
  in
  expect_error [] Board.NoCards;
  expect_error [0; 1; 2; 3; 4; 5] (Board.TooManyCards 6);
  expect_error [8] (Board.InvalidIndex 8);
  expect_error [-1] (Board.InvalidIndex (-1));
  expect_error [3; 3] (Board.DuplicateIndex 3)
;;

(* Discarding and refilling until the deck runs out must not raise. *)
let test_empty_deck color =
  let rec loop board =
    match Deck.cards (Board.deck board) with
    | [] -> board
    | _ ->
      (match Board.discard [0; 1; 2; 3; 4] board with
       | Ok board -> loop (Board.refill board)
       | Error e -> failwith (Board.show_error e))
  in
  let board = fresh_board color |> Board.refill |> loop in
  (* No card was lost or duplicated along the way. *)
  assert (List.length (Board.hand board) + List.length (Board.discarded board) = 52);
  (* Refilling with an empty deck leaves the hand as it is. *)
  assert (Board.hand (Board.refill board) = Board.hand board)
;;

let () =
  List.iter
    (fun color ->
       test_refill color;
       test_play_and_discard color;
       test_invalid_selections color;
       test_empty_deck color)
    [Deck.Red; Deck.Blue; Deck.Green]
;;
