open Base
open Consumables
module HandMap : Map.S with type key = Poker_hand.t

type record = {
  hand: Poker_hand.t;
  level: int;
  scoring_effect: Score.t;
}

val planets : Planet.t HandMap.t
val score : Planet.t -> Poker_hand.t -> Score.change list
val consume : Planet.t -> record -> record
val base_hands : (Poker_hand.t * record) list
