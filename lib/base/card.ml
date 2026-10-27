open Scoring

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

type t = { suit : suit; rank : rank; decorations : Effect.decorations }

let create suit rank = { suit; rank; decorations = Effect.no_decorations }
let suit { suit; rank } = suit
let rank { suit; rank } = rank
let all_suits = [ Clubs; Diamonds; Hearts; Spades ]

let all_ranks =
  [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;
  ]
