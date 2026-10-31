(** Jokers change the score of every play while they are owned. Each joker is a
    constructor; what it does, what it costs and how rare it is are functions on the
    type, so jokers can be compared, printed and listed with [all]. *)

type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary
[@@deriving show, eq]

(** A first set of jokers; more can be added one constructor at a time. *)
type t =
  | Joker (** +4 mult. *)
  | GreedyJoker (** +3 mult for every scoring diamond. *)
  | LustyJoker (** +3 mult for every scoring heart. *)
  | WrathfulJoker (** +3 mult for every scoring spade. *)
  | GluttonousJoker (** +3 mult for every scoring club. *)
  | JollyJoker (** +8 mult if the hand is a [Pair]. *)
  | HalfJoker (** +20 mult if 3 or fewer cards were played. *)
  | Banner (** +30 chips for every discard left. *)
  | AbstractJoker (** +3 mult for every joker owned. *)
  | Cavendish (** x3 mult. *)
[@@deriving show, eq, enumerate]

val rarity : t -> rarity

(** Price in the shop. *)
val cost : t -> int

(** What selling gives back: half of [cost], rounded down, and never less than $1. *)
val sell_value : t -> int

(** What a joker can see when it scores. A record so it can grow without changing
    every caller. *)
type context = {
  play: Play.t;
  discards_left: int;
  jokers: t list; (** Every joker owned, in order, including the one scoring. *)
}

(** The changes one joker makes to the score of a play. Most jokers return a single
    change, or none when their condition does not hold. *)
val changes : context -> t -> Score.change list

(** The changes of every joker in [context.jokers], left to right, in the order they
    are applied. *)
val all_changes : context -> Score.change list
