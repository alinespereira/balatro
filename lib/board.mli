type t

val create : Random.State.t -> Deck.color -> t
val deck : t -> Deck.t
val hand : t -> Card.t list
val refill : Random.State.t -> t -> t
val play : int list -> t -> (Play.t * t, Poker.Hand.error) result
val discard : int list -> t -> (t, Poker.Hand.error) result