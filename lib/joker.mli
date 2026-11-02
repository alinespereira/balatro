(** Jokers change the score of every play while they are owned. Each joker is a
    constructor; what it does, what it costs and how rare it is are functions on the
    type, so jokers can be compared, printed and listed with [all].

    Some jokers have state: a number that grows or shrinks during the run. The state
    is the payload of the constructor ([GreenJoker { mult = 3 }]), and [react] gives
    back the joker changed by what happened in the blind. *)

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
  | JollyJoker (** +8 mult if the hand contains a pair ([Poker.Hand.contains_pair]). *)
  | HalfJoker (** +20 mult if 3 or fewer cards were played. *)
  | Banner (** +30 chips for every discard left. *)
  | AbstractJoker (** +3 mult for every joker owned. *)
  | Cavendish (** x3 mult. *)
  | GreenJoker of { mult: int }
  (** +[mult] mult. Gains 1 for every hand played, loses 1 for every discard, never
      below 0. *)
  | RideTheBus of { mult: int }
  (** +[mult] mult. Gains 1 for every hand played in a row without a scoring face
      card (J, Q, K); a scoring face card brings it back to 0. *)
  | Runner of { chips: int }
  (** +[chips] chips. Gains 15 for every hand played that contains a straight. *)
  | IceCream of { chips: int }
  (** +[chips] chips. Loses 5 for every hand played, and melts (goes away) at 0. *)
[@@deriving show, eq]

(** Every joker once, as the shop sells it: the ones with state start fresh. Written
    by hand, because [ppx_enumerate] cannot list every [int] in a payload. *)
val all : t list

(** The joker as it comes from the shop: the same constructor, with its state back
    to the start. Jokers without state are left as they are. *)
val fresh : t -> t

(** Whether two jokers are the same joker, whatever their state:
    [same_kind (GreenJoker { mult = 5 }) (GreenJoker { mult = 0 })] is [true]. *)
val same_kind : t -> t -> bool

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

(** Something that happened in the blind, that a joker with state may react to. *)
type event =
  | Played of Play.t
  (** A hand was played and has already scored. Under a boss, the play is the one
      the jokers saw: without the debuffed cards. *)
  | Discarded (** Cards were discarded. *)

(** The joker after the event, or [None] when the event uses it up (an [IceCream] that
    melts). Jokers without state come back unchanged. *)
val react : event -> t -> t option
