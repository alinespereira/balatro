module PlanetScoring = Consumables.Planet
module Planets = Base.Consumables.Planet
module HandMap = Map.Make (Poker_hand)
open Poker_hand

type record = {
  hand: Poker_hand.t;
  level: int;
  scoring_effect: Score.t;
}

let base_hands =
  Poker_hand.all
  |> List.rev
  |> List.map (fun hand ->
    ( hand,
      { hand; level = 1; scoring_effect = Poker_hand.base_score hand } ))
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

let consume planet (hand : record) =
  match score planet hand.hand with
  | [] -> hand
  | changes ->
    let scoring_effect = Score.apply_all changes hand.scoring_effect in
    { hand with level = hand.level + 1; scoring_effect }
;;
