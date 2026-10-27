module HandMap : Map.S with type key = Base.Hand.t

val planets : Base.Consumables.Planet.t HandMap.t
val score : Base.Consumables.Planet.t -> Base.Hand.t -> Base.Effect.change list
val consume : Base.Consumables.Planet.t -> Base.Hand.record -> Base.Hand.record

val base_scores : Base.Effect.t HandMap.t

val base_hands : (Base.Hand.t * Base.Hand.record) list

val is_full_house : Base.Card.t list -> bool
(** Whether exactly five cards contain three of one rank and two of another.
    Suits and decorations are ignored. *)
