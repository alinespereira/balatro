type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades
[@@deriving show, eq, ord]

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
[@@deriving show, eq, ord]

type t = {
  suit: suit;
  rank: rank;
  decoration: Decoration.t;
}

val create : suit -> rank -> t
val all_suits : suit list
val all_ranks : rank list

module RankMap : Map.S with type key = rank
module SuitMap : Map.S with type key = suit
