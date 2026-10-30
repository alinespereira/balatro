type t = {
  deck: Deck.t;
  hand: Card.t list;
  discarded: Card.t list;
  jokers: Joker.t list;
  consumables: Consumables.Consumable.t list;
  hand_size: int;
  joker_slots: int;
  consumable_slots: int;
}

let deck { deck; _ } = deck
let hand { hand; _ } = hand
let hand_size = 1
let joker_slots = 5
let consumable_slots = 2

let create rng color =
  let deck = Deck.create color |> Deck.shuffle rng in
  {
    deck;
    hand = [];
    discarded = [];
    jokers = [];
    consumables = [];
    hand_size;
    joker_slots;
    consumable_slots;
  }
;;

let refill rng board =
  let rec loop board size =
    if size <= 0 then
      board
    else (
      let sample, rest = board |> deck |> Deck.cards |> Util.draw rng in
      let board =
        { board with deck = Deck.set_cards rest board.deck; hand = sample :: board.hand }
      in
      loop board (size - 1)
    )
  in
  loop board (board.hand_size - List.length board.hand)
;;

let select indices hand =
  let size = List.length hand in
  let rec validate seen = function
    | [] -> Ok ()
    | idx :: rest ->
      if idx < 0 || idx >= size then
        Error (Poker.Hand.InvalidIndex idx)
      else if List.mem idx seen then
        Error (Poker.Hand.DuplicateIndex idx)
      else
        validate (idx :: seen) rest
  in
  match indices with
  | [] -> Error Poker.Hand.NoCards
  | _ ->
    validate [] indices
    |> Result.map (fun () ->
      let chosen, kept =
        hand
        |> List.mapi (fun pos card -> pos, card)
        |> List.partition (fun (pos, _) -> List.mem pos indices)
      in
      List.map snd chosen, List.map snd kept)
;;

let play indices board =
  Result.map
    (fun (selected, kept) ->
       let new_board =
         { board with discarded = selected @ board.discarded; hand = kept }
       in
       match Play.make selected with
       | Ok play -> play, new_board
       | Error e -> Util.not_implemented)
    (select indices board.hand)
;;

let discard indices board =
  Result.map
    (fun (selected, kept) ->
       { board with discarded = selected @ board.discarded; hand = kept })
    (select indices board.hand)
;;
