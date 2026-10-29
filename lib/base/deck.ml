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
      (fun suit -> List.map (fun rank -> Card.create suit rank) Card.all_ranks)
      Card.all_suits
  in
  { cards; color }
;;

let cards { cards } = cards
let set_cards cards deck = { deck with cards }
