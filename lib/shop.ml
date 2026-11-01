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

let available ~jokers ~levels =
  all_items
  |> List.filter (fun item ->
    match item with
    | JokerItem joker -> not (List.exists (Joker.same_kind joker) jokers)
    | PlanetItem planet ->
      (* A planet shows up if one of its hands is not secret or was already played. *)
      Poker.Hand.all
      |> List.exists (fun hand ->
        let played = Poker.Levels.played (Poker.Levels.get hand levels) in
        Consumables.Planet.equal (Poker.Hand.planet hand) planet
        && ((not (Poker.Hand.is_secret hand)) || played > 0)))
;;

let draw_offers rng from = from |> Util.shuffle rng |> List.take offer_count

let create ?(from = all_items) rng =
  let offers = draw_offers rng from in
  { offers; reroll_cost = base_reroll_cost }
;;

let take idx shop =
  let { offers; _ } = shop in
  if idx < 0 then
    Error (NoOfferAt idx)
  else (
    match List.nth_opt offers idx with
    | Some offer ->
      Ok (offer, { shop with offers = List.filteri (fun i _ -> i <> idx) offers })
    | None -> Error (NoOfferAt idx)
  )
;;

let reroll ?(from = all_items) rng shop =
  let new_offers = draw_offers rng from in
  let { reroll_cost; _ } = shop in
  { offers = new_offers; reroll_cost = reroll_cost + 1 }
;;
