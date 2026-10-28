type suit = Clubs | Diamonds | Hearts | Spades
[@@deriving show, eq, enum, ord]

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
[@@deriving show, eq, enum, ord]

type t = { suit : suit; rank : rank; decorations : Decoration.t }

let create suit rank = { suit; rank; decorations = Decoration.no_decorations }
let suit { suit; rank } = suit
let rank { suit; rank } = rank
let all_suits = Util.list_of_enum min_suit max_suit suit_of_enum
let all_ranks = Util.list_of_enum min_rank max_rank rank_of_enum

module RankPair = struct
  type t = rank

  let compare = compare_rank
end

module RankMap = Map.Make (RankPair)

module SuitPair = struct
  type t = suit

  let compare = compare_suit
end

module SuitMap = Map.Make (SuitPair)
