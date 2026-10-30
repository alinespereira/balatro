(** A valid play: the hand the cards make and the cards that score. The type is abstract
    so the only way to get one is [make], which means every [t] is a valid play. *)
type t

val make : Card.t list -> (t, Poker.Hand.error) result
val hand : t -> Poker.Hand.t
val scoring_cards : t -> Card.t list

(** Chips and mult of the play before jokers, at the hand's current level. *)
val score : Poker.Levels.t -> t -> Score.t
