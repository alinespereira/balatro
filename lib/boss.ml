type t =
  | Drought
  | Hourglass
  | Giant
  | Eclipse of Card.suit
  | Fog
  | Muzzle
[@@deriving show, eq, enumerate]

type limits = {
  plays: int;
  discards: int;
  target: int;
}
[@@deriving show, eq]

let random rng = List.nth all (Random.State.int rng (List.length all))

(* TODO: return the limits changed by the boss. A record update, such as
   [{ limits with discards = 0 }], copies every field except the ones it names. *)
let limits _boss _limits = failwith "TODO: Boss.limits"

(* TODO: [Eclipse suit] debuffs the cards of [suit], [Fog] the face cards, and the
   other bosses none. Pattern matching can look inside [Eclipse] and name the suit. *)
let debuffs _boss _card = failwith "TODO: Boss.debuffs"

(* TODO: only [Muzzle]. *)
let disables_jokers _boss = failwith "TODO: Boss.disables_jokers"
