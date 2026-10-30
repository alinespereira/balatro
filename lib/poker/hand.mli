(** Poker hand categories, how to detect them, and what they score at
    level 1. *)

type t =
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  | FiveOfAKind
  | FlushHouse
  | FlushFive
[@@deriving show, eq, ord, enum, enumerate]

(** Chips and mult of the category at level 1. *)
val base_score : t -> Score.t

(** The planet that levels up the hand. Straight flush and royal flush share
    Neptune. *)
val planet : t -> Consumables.Planet.t

type evaluation = {
  category: t;
  scoring_cards: Card.t list;
}

type error = InvalidCardCount of int

val evaluate : Card.t list -> (evaluation, error) result

(** The [is_*] checks are exposed so they can be tested while [evaluate] is
    not implemented. Once it is, they can become internal to this module. *)

val is_high_card : Card.t list -> bool
val is_pair : Card.t list -> bool
val is_two_pair : Card.t list -> bool
val is_three_of_a_kind : Card.t list -> bool
val is_straight : Card.t list -> bool
val is_flush : Card.t list -> bool
val is_full_house : Card.t list -> bool
val is_four_of_a_kind : Card.t list -> bool
val is_straight_flush : Card.t list -> bool
val is_royal_flush : Card.t list -> bool
val is_five_of_a_kind : Card.t list -> bool
val is_flush_house : Card.t list -> bool
val is_flush_five : Card.t list -> bool
