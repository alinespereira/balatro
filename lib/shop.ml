type item =
  | JokerItem of Joker.t
  | PlanetItem of Consumables.Planet.t
[@@deriving show, eq]

type t = {
  offers: item list;
  reroll_cost: int;
}

type error = NoOfferAt of int [@@deriving show]

let offer_count = 2
let base_reroll_cost = 5

let price = function
  | JokerItem joker -> Joker.cost joker
  | PlanetItem _ -> 3
;;

let all_items =
  List.map (fun joker -> JokerItem joker) Joker.all
  @ List.map (fun planet -> PlanetItem planet) Consumables.Planet.all
;;

(* TODO: draw [offer_count] items from [all_items] without repeats. [Util.draw] returns
   the item drawn and the list without it. *)
let create _ = failwith "Shop.create: not implemented"

(* TODO: [NoOfferAt] for a position out of range, as in [Board.remove_joker]. *)
let take _ _ = failwith "Shop.take: not implemented"

(* TODO: new offers like [create], and [reroll_cost + 1]. *)
let reroll _ _ = failwith "Shop.reroll: not implemented"
