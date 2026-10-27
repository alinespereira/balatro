type t =
  | High_card
  | Pair
  | Two_pair
  | Three_of_a_kind
  | Straight
  | Flush
  | Full_house
  | Four_of_a_kind
  | Straight_flush
  | Royal_flush
  | Five_of_a_kind
  | Flush_house
  | Flush_five
[@@deriving show, eq, enum, ord]

type record = { hand : t; level : int; scoring_effect : Effect.t }

