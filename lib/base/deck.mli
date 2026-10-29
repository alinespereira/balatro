type t

type color =
  | Red
  | Blue
  | Green

val create : color -> t
val cards : t -> Card.t list
val set_cards : Card.t list -> t -> t
