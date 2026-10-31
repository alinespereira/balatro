type color =
  | Red
  | Blue
  | Green

type t = private {
  cards: Card.t list;
  color: color;
}

val create : color -> t
val shuffle : Random.State.t -> t -> t
val cards : t -> Card.t list
val color : t -> color
val set_cards : Card.t list -> t -> t
