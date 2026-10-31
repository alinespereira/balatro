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

module RankMap : Map.S with type key = rank
module SuitMap : Map.S with type key = suit
