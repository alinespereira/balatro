type t = {
  blind: int;
  plays: int;
  interest: int;
}
[@@deriving show, eq]

let max_interest = 5

(* TODO: $1 per play left, and $1 for every $5 of [money] up to [max_interest]. *)
let make ~reward:_ ~plays_left:_ ~money:_ = failwith "Payout.make: not implemented"

(* TODO *)
let total _ = failwith "Payout.total: not implemented"
