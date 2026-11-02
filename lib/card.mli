type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades
[@@deriving show, eq, ord, enum, enumerate]

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
[@@deriving show, eq, ord, enum, enumerate]

type t = private {
  suit: suit;
  rank: rank;
  decoration: Decoration.t;
}

val create : suit -> rank -> t

(** The same card with another suit. The decoration stays. *)
val with_suit : suit -> t -> t

(** The same card with another rank. The decoration stays. *)
val with_rank : rank -> t -> t

(** The rank right above: [Two] gives [Three], [King] gives [Ace], and [Ace] goes back
    to [Two]. *)
val next_rank : rank -> rank

module RankMap : Map.S with type key = rank
module SuitMap : Map.S with type key = suit
