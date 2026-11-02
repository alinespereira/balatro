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
let with_suit suit card = { card with suit }
let with_rank rank card = { card with rank }

let next_rank = function
  | Ace -> Two
  | rank -> rank |> rank_to_enum |> ( + ) 1 |> rank_of_enum |> Option.get
;;

module RankMap = Map.Make (struct
    type t = rank

    let compare = compare_rank
  end)

module SuitMap = Map.Make (struct
    type t = suit

    let compare = compare_suit
  end)
