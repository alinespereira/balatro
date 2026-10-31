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

let create rng =
  let offers = all_items |> Util.shuffle rng |> List.take offer_count in
  { offers; reroll_cost = base_reroll_cost }
;;

let take idx shop =
  let { offers; _ } = shop in
  if idx < 0 || idx >= List.length offers then
    Error (NoOfferAt idx)
  else (
    match List.nth_opt offers idx with
    | Some offer ->
      Ok (offer, { shop with offers = List.filteri (fun i _ -> i <> idx) offers })
    | None -> Error (NoOfferAt idx)
  )
;;

let reroll rng shop =
  let new_offers = all_items |> Util.shuffle rng |> List.take offer_count in
  let { reroll_cost; _ } = shop in
  { offers = new_offers; reroll_cost = reroll_cost + 1 }
;;
