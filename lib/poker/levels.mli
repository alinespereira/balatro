(** The level and current score of every poker hand during a run. *)

(** Code outside this module can read an entry but not build one, so
    levels only change through [use_planet] and [record_play]. *)
type entry = private {
  level: int;
  score: Score.t;
  played: int; (** How many times the hand was played in this run. *)
}

(** Abstract: every value of [t] has an entry for every hand, which is what
    makes [get] safe. *)
type t

(** Every hand at level 1 with its base score, never played. *)
val initial : t

val get : Hand.t -> t -> entry

(** How many times the hand of this entry was played in this run. *)
val played : entry -> int

(** Levels up every hand whose planet is the given one. *)
val use_planet : Consumables.Planet.t -> t -> t

(** Counts one more play of the hand. Its level and score stay the same. *)
val record_play : Hand.t -> t -> t
