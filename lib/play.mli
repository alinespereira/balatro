(** A valid play: the hand the cards make and the played cards. The type is private
    so the only way to get one is [make], which means every [t] is a valid play. *)
type t = private {
  evaluation: Poker.Hand.evaluation;
  cards: Card.t list;
}

val make : Card.t list -> (t, Poker.Hand.error) result
val hand : t -> Poker.Hand.t
val scoring_cards : t -> Card.t list

(** Every card that was played, the ones that score and the ones that do not, in the
    order they were played. *)
val cards : t -> Card.t list

(** Chips and mult of the play before jokers, at the hand's current level. *)
val score : Poker.Levels.t -> t -> Score.t
