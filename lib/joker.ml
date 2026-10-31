type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary
[@@deriving show, eq]

type t =
  | Joker
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | JollyJoker
  | HalfJoker
  | Banner
  | AbstractJoker
  | Cavendish
[@@deriving show, eq, enumerate]

(* All the jokers so far are common in Balatro. *)
let rarity _ = Common

let cost = function
  | Joker -> 2
  | JollyJoker -> 3
  | AbstractJoker
  | Cavendish ->
    4
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | HalfJoker
  | Banner ->
    5
;;

(* TODO *)
let sell_value _ = failwith "Joker.sell_value: not implemented"

type context = {
  play: Play.t;
  discards_left: int;
  jokers: t list;
}

(* TODO: one case per joker. [Play.scoring_cards], [Play.cards] and [Play.hand] have
   what the conditions need. *)
let changes _ _ = failwith "Joker.changes: not implemented"

(* TODO *)
let all_changes _ = failwith "Joker.all_changes: not implemented"
