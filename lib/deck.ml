type color =
  | Red
  | Blue
  | Green

type t = {
  cards: Card.t list;
  color: color;
}

let create color =
  let cards =
    List.concat_map
      (fun suit -> List.map (fun rank -> Card.create suit rank) Card.all_of_rank)
      Card.all_of_suit
  in
  { cards; color }
;;

let shuffle rng deck = { deck with cards = Util.shuffle rng deck.cards }
let cards { cards; _ } = cards
let set_cards cards deck = { deck with cards }
