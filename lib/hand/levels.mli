(** The level and current score of every poker hand during a run. *)

(** Code outside this module can read an entry but not build one, so
    levels only change through [use_planet]. *)
type entry = private {
  level: int;
  score: Score.t;
}

(** Abstract: every value of [t] has an entry for every hand, which is what
    makes [get] safe. *)
type t

(** Every hand at level 1 with its base score. *)
val initial : t

val get : Poker.Hand.t -> t -> entry

(** Levels up every hand whose planet is the given one. *)
val use_planet : Consumables.Planet.t -> t -> t
