type suit = Clubs | Diamonds | Hearts | Spades

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

type t = { suit : suit; rank : rank; decorations : Scoring_effect.decorations }

let create suit rank =
  { suit; rank; decorations = Scoring_effect.no_decorations }

let suit { suit; rank } = suit
let rank { suit; rank } = rank
let all_suits = [ Clubs; Diamonds; Hearts; Spades ]

let all_ranks =
  [
    Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace;
  ]
