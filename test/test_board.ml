open Balatro

let test_board color =
  let rng = Random.State.make [| 42 |] in
  let board = Board.create rng color in
  let original_cards = Deck.cards (Board.deck board) in
  assert (List.length original_cards = 52);
  assert (Board.hand board = []);
  let filled = Board.refill rng board in
  let drawn = Board.hand filled in
  let remaining = Deck.cards (Board.deck filled) in
  assert (List.length drawn = 1);
  assert (List.length remaining = 51);
  let same_card (a : Card.t) (b : Card.t) = a.suit = b.suit && a.rank = b.rank in
  List.iter
    (fun card ->
       let occurrences = List.filter (same_card card) (drawn @ remaining) in
       assert (List.length occurrences = 1))
    original_cards;
  (* Filling an already full draw must not consume more cards. *)
  let refilled = Board.refill rng filled in
  assert (Board.hand refilled = drawn);
  assert (Deck.cards (Board.deck refilled) = remaining);
  (* The original board remains unchanged. *)
  assert (Board.hand board = []);
  assert (Deck.cards (Board.deck board) = original_cards)
;;

let () = List.iter test_board [Deck.Red; Deck.Blue; Deck.Green]
