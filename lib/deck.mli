type t

type color =
  | Red
  | Blue
  | Green

val create : color -> t
val shuffle : Random.State.t -> t -> t
val cards : t -> Card.t list
val color : t -> color
val set_cards : Card.t list -> t -> t
