type t

val create: Deck.color -> t
val fill_draw: t -> t

val deck : t -> Deck.t
val draw : t -> Card.t list
