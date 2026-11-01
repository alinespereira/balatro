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

(** The same play without the scoring cards that [debuffed] picks: they stay in
    [cards], but no longer score. The hand stays the same, so a pair with a debuffed
    card is still a pair. *)
val without : (Card.t -> bool) -> t -> t

(** Chips and mult of the play before jokers, at the hand's current level. *)
val score : Poker.Levels.t -> t -> Score.t
