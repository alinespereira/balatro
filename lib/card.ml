type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades
[@@deriving show, eq, ord, enum, enumerate]

type rank =
  | Two [@value 2]
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

type t = {
  suit: suit;
  rank: rank;
  decoration: Decoration.t;
}

let create suit rank = { suit; rank; decoration = Decoration.none }

module RankMap = Map.Make (struct
    type t = rank

    let compare = compare_rank
  end)

module SuitMap = Map.Make (struct
    type t = suit

    let compare = compare_suit
  end)
