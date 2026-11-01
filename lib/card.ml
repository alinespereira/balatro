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

(* TODO: a copy of the record with only [suit] changed: [{ card with ... }]. *)
let with_suit _suit _card = failwith "TODO: Card.with_suit"

(* TODO: like [with_suit], for the rank. *)
let with_rank _rank _card = failwith "TODO: Card.with_rank"

(* TODO: [rank_to_enum] and [rank_of_enum] come from [@@deriving enum]; [Two] is 2 and
   [Ace] is 14. [rank_of_enum] returns an option: [None] past the [Ace]. *)
let next_rank _rank = failwith "TODO: Card.next_rank"

module RankMap = Map.Make (struct
    type t = rank

    let compare = compare_rank
  end)

module SuitMap = Map.Make (struct
    type t = suit

    let compare = compare_suit
  end)
