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
[@@deriving show, eq, ord]

(** Every category, from weakest to strongest. *)
val all : t list

(** Chips and mult of the category at level 1. *)
val base_score : t -> Score.t

type evaluation = {
  category: t;
  scoring_cards: Base.Card.t list;
}

type error = InvalidCardCount of int

val evaluate : Base.Card.t list -> (evaluation, error) result

(** The [is_*] checks are exposed so they can be tested while [evaluate] is
    not implemented. Once it is, they can become internal to this module. *)

val is_high_card : Base.Card.t list -> bool
val is_pair : Base.Card.t list -> bool
val is_two_pair : Base.Card.t list -> bool
val is_three_of_a_kind : Base.Card.t list -> bool
val is_straight : Base.Card.t list -> bool
val is_flush : Base.Card.t list -> bool
val is_full_house : Base.Card.t list -> bool
val is_four_of_a_kind : Base.Card.t list -> bool
val is_straight_flush : Base.Card.t list -> bool
val is_royal_flush : Base.Card.t list -> bool
val is_five_of_a_kind : Base.Card.t list -> bool
val is_flush_house : Base.Card.t list -> bool
val is_flush_five : Base.Card.t list -> bool
