(** The boss of an ante: a rule that makes its [Boss] blind harder. The run draws one
    boss for every ante, so the player sees it coming during the [Small] and [Big]
    blinds.

    A boss changes a blind in two ways: before it starts, through [limits] (fewer plays,
    no discards, a bigger target), and on every play, through [debuffs] (cards that do
    not score) and [disables_jokers]. *)

type t =
  | Drought (** No discards. *)
  | Hourglass (** One play fewer, but never less than one. *)
  | Giant (** The target is 50% bigger, rounded down. *)
  | Eclipse of Card.suit (** Cards of this suit do not score. *)
  | Fog (** Face cards (J, Q, K) do not score. *)
  | Muzzle (** Jokers do nothing. *)
[@@deriving show, eq, enumerate]
(* [enumerate] builds [all] by itself. For [Eclipse of Card.suit] it uses
   [Card.all_of_suit], so [all] has one [Eclipse] for each suit. *)

(** What a blind starts with. *)
type limits = {
  plays: int;
  discards: int;
  target: int;
}
[@@deriving show, eq]

(** One of [all], drawn with [rng]. *)
val random : Random.State.t -> t

(** The limits of a blind with this boss, given the limits it would have without one. *)
val limits : t -> limits -> limits

(** Whether this boss keeps the card from scoring. A debuffed card gives no chips, and
    jokers do not see it either. *)
val debuffs : t -> Card.t -> bool

(** Whether this boss turns every joker off. *)
val disables_jokers : t -> bool
