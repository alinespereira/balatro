module PlanetScoring = Consumables.Planet
open Base.Hand
module Planets = Base.Consumables.Planet
module HandMap = Map.Make (Base.Hand)

type evaluation = { category : Base.Hand.t; scoring_cards : Base.Card.t list }
type error = Invalid_card_count of int

let base_scores =
  HandMap.empty
  |> HandMap.add Flush_five (Base.Effect.create ~chips:160 ~mult:16)
  |> HandMap.add Flush_house (Base.Effect.create ~chips:140 ~mult:14)
  |> HandMap.add Five_of_a_kind (Base.Effect.create ~chips:120 ~mult:12)
  |> HandMap.add Royal_flush (Base.Effect.create ~chips:100 ~mult:8)
  |> HandMap.add Straight_flush (Base.Effect.create ~chips:100 ~mult:8)
  |> HandMap.add Four_of_a_kind (Base.Effect.create ~chips:60 ~mult:7)
  |> HandMap.add Full_house (Base.Effect.create ~chips:40 ~mult:4)
  |> HandMap.add Flush (Base.Effect.create ~chips:35 ~mult:4)
  |> HandMap.add Straight (Base.Effect.create ~chips:30 ~mult:4)
  |> HandMap.add Three_of_a_kind (Base.Effect.create ~chips:30 ~mult:3)
  |> HandMap.add Two_pair (Base.Effect.create ~chips:20 ~mult:2)
  |> HandMap.add Pair (Base.Effect.create ~chips:10 ~mult:2)
  |> HandMap.add High_card (Base.Effect.create ~chips:5 ~mult:1)

let base_hands =
  let make_hand (hand, scoring_effect) =
    (hand, { hand; level = 1; scoring_effect })
  in
  base_scores |> HandMap.bindings |> List.rev |> List.map make_hand

let planets =
  HandMap.empty
  |> HandMap.add High_card Planets.Pluto
  |> HandMap.add Pair Planets.Mercury
  |> HandMap.add Two_pair Planets.Uranus
  |> HandMap.add Three_of_a_kind Planets.Venus
  |> HandMap.add Straight Planets.Saturn
  |> HandMap.add Flush Planets.Jupiter
  |> HandMap.add Full_house Planets.Earth
  |> HandMap.add Four_of_a_kind Planets.Mars
  |> HandMap.add Straight_flush Planets.Neptune
  |> HandMap.add Royal_flush Planets.Neptune

let score planet hand =
  match HandMap.find_opt hand planets with
  | Some matching when Base.Consumables.Planet.equal matching planet ->
      PlanetScoring.PlanetMap.find planet PlanetScoring.scores
  | _ -> []

let consume planet (hand : Base.Hand.record) =
  match score planet hand.hand with
  | [] -> hand
  | changes ->
      let scoring_effect =
        List.fold_left
          (fun current change -> Effect.apply change current)
          hand.scoring_effect changes
      in
      { hand with level = hand.level + 1; scoring_effect }

let group_by_rank = Util.groups ~key:Base.Card.rank
let group_by_suit = Util.groups ~key:Base.Card.suit

let rank_counts cards =
  let groups = group_by_rank cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare

let suit_counts cards =
  let groups = group_by_suit cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare

let evaluate _ = failwith "Not implemented"
let is_high_card cards = not (List.is_empty cards)
let is_pair cards = rank_counts cards = [ 1; 1; 1; 2 ]
let is_two_pair cards = rank_counts cards = [ 1; 2; 2 ]
let is_three_of_a_kind cards = rank_counts cards = [ 1; 1; 2 ]
let is_straight _ = failwith "Not implemented"
let is_flush _ = failwith "Not implemented"
let is_full_house cards = rank_counts cards = [ 2; 3 ]
(* let add_rank counts card =
    let rank = Base.Card.rank card in
    let count = Option.value (List.assoc_opt rank counts) ~default:0 in
    (rank, count + 1) :: List.remove_assoc rank counts
  in
  List.length cards = 5
  && cards |> List.fold_left add_rank [] |> List.map snd
     |> List.sort Int.compare = [ 2; 3 ] *)

let is_four_of_a_kind _ = failwith "Not implemented"
let is_straight_flush _ = failwith "Not implemented"
let is_royal_flush _ = failwith "Not implemented"
let is_five_of_a_kind _ = failwith "Not implemented"
let is_flush_house _ = failwith "Not implemented"
let is_flush_five _ = failwith "Not implemented"

let get_high_card cards =
  cards
  |> List.sort (fun card_1 card_2 ->
      Base.Card.compare_suit (Base.Card.suit card_2) (Base.Card.suit card_1))
  |> List.hd
