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

type record = {
  hand: t;
  level: int;
  scoring_effect: Score.t;
}
