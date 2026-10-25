open Balatro

let test_board color =
  let board = Board.create color in
  let original_cards = Deck.cards (Board.deck board) in
  assert (List.length original_cards = 52);
  assert (Board.draw board = []);
  let filled = Board.fill_draw board in
  let drawn = Board.draw filled in
  let remaining = Deck.cards (Board.deck filled) in
  assert (List.length drawn = 1);
  assert (List.length remaining = 51);
  let same_card a b = Card.suit a = Card.suit b && Card.rank a = Card.rank b in
  List.iter
    (fun card ->
      let occurrences = List.filter (same_card card) (drawn @ remaining) in
      assert (List.length occurrences = 1))
    original_cards;
  (* Filling an already full draw must not consume more cards. *)
  let refilled = Board.fill_draw filled in
  assert (Board.draw refilled = drawn);
  assert (Deck.cards (Board.deck refilled) = remaining);
  (* The original board remains unchanged. *)
  assert (Board.draw board = []);
  assert (Deck.cards (Board.deck board) = original_cards)

let () = List.iter test_board [ Deck.Red; Deck.Blue; Deck.Green ]
