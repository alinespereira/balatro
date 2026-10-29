module PlanetScoring = Consumables.Planet
open Base.Hand
module Planets = Base.Consumables.Planet
module HandMap = Map.Make (Base.Hand)

type evaluation = {
  category: Base.Hand.t;
  scoring_cards: Base.Card.t list;
}

type error = InvalidCardCount of int

let base_scores =
  HandMap.empty
  |> HandMap.add FlushFive (Score.create ~chips:160 ~mult:16.)
  |> HandMap.add FlushHouse (Score.create ~chips:140 ~mult:14.)
  |> HandMap.add FiveOfAKind (Score.create ~chips:120 ~mult:12.)
  |> HandMap.add RoyalFlush (Score.create ~chips:100 ~mult:8.)
  |> HandMap.add StraightFlush (Score.create ~chips:100 ~mult:8.)
  |> HandMap.add FourOfAKind (Score.create ~chips:60 ~mult:7.)
  |> HandMap.add FullHouse (Score.create ~chips:40 ~mult:4.)
  |> HandMap.add Flush (Score.create ~chips:35 ~mult:4.)
  |> HandMap.add Straight (Score.create ~chips:30 ~mult:4.)
  |> HandMap.add ThreeOfAKind (Score.create ~chips:30 ~mult:3.)
  |> HandMap.add TwoPair (Score.create ~chips:20 ~mult:2.)
  |> HandMap.add Pair (Score.create ~chips:10 ~mult:2.)
  |> HandMap.add HighCard (Score.create ~chips:5 ~mult:1.)
;;

let base_hands =
  let make_hand (hand, scoring_effect) = hand, { hand; level = 1; scoring_effect } in
  base_scores |> HandMap.bindings |> List.rev |> List.map make_hand
;;

let planets =
  HandMap.empty
  |> HandMap.add HighCard Planets.Pluto
  |> HandMap.add Pair Planets.Mercury
  |> HandMap.add TwoPair Planets.Uranus
  |> HandMap.add ThreeOfAKind Planets.Venus
  |> HandMap.add Straight Planets.Saturn
  |> HandMap.add Flush Planets.Jupiter
  |> HandMap.add FullHouse Planets.Earth
  |> HandMap.add FourOfAKind Planets.Mars
  |> HandMap.add StraightFlush Planets.Neptune
  |> HandMap.add RoyalFlush Planets.Neptune
;;

let score planet hand =
  match HandMap.find_opt hand planets with
  | Some matching when Base.Consumables.Planet.equal matching planet ->
    PlanetScoring.PlanetMap.find planet PlanetScoring.scores
  | _ -> []
;;

let consume planet (hand : Base.Hand.record) =
  match score planet hand.hand with
  | [] -> hand
  | changes ->
    let scoring_effect = Score.apply_all changes hand.scoring_effect in
    { hand with level = hand.level + 1; scoring_effect }
;;

let group_by_rank = Util.groups ~key:(fun (card : Base.Card.t) -> card.rank)
let group_by_suit = Util.groups ~key:(fun (card : Base.Card.t) -> card.suit)

let rank_counts cards =
  let groups = group_by_rank cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare
;;

let suit_counts cards =
  let groups = group_by_suit cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare
;;

let evaluate _ = failwith "Not implemented"
let is_high_card cards = not (List.is_empty cards)
let is_pair cards = rank_counts cards = [1; 1; 1; 2]
let is_two_pair cards = rank_counts cards = [1; 2; 2]
let is_three_of_a_kind cards = rank_counts cards = [1; 1; 2]
let is_straight _ = failwith "Not implemented"
let is_flush _ = failwith "Not implemented"
let is_full_house cards = rank_counts cards = [2; 3]
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
  |> List.sort (fun (card_1 : Base.Card.t) (card_2 : Base.Card.t) ->
    Base.Card.compare_suit card_2.suit card_1.suit)
  |> List.hd
;;
