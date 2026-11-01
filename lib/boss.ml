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

let random ?(avoid = []) rng =
  let candidates =
    match List.filter (fun boss -> not (List.mem boss avoid)) all with
    | [] -> all
    | filtered -> filtered
  in
  candidates |> Util.draw rng |> fst
;;

let limits boss limits =
  let { plays = the_plays; target = the_target; _ } = limits in
  match boss with
  | Drought -> { limits with discards = 0 }
  | Hourglass -> { limits with plays = max 1 (the_plays - 1) }
  | Giant -> { limits with target = the_target * 3 / 2 }
  | Eclipse _
  | Fog
  | Muzzle ->
    limits
;;

let debuffs boss card =
  match boss, card with
  | Drought, _
  | Hourglass, _
  | Giant, _ ->
    false
  | Eclipse boss_suit, { Card.suit = card_suit; _ } -> boss_suit = card_suit
  | Fog, { Card.rank = Card.Jack | Card.Queen | Card.King; _ } -> true
  | Fog, _
  | Muzzle, _ ->
    false
;;

let disables_jokers = function
  | Drought
  | Hourglass
  | Giant
  | Eclipse _
  | Fog ->
    false
  | Muzzle -> true
;;
