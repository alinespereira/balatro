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

let create suit rank = { suit; rank; decoration = Decoration.none }
let all_suits = [Clubs; Diamonds; Hearts; Spades]

let all_ranks =
  [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace]
;;

module RankMap = Map.Make (struct
    type t = rank

    let compare = compare_rank
  end)

module SuitMap = Map.Make (struct
    type t = suit

    let compare = compare_suit
  end)
