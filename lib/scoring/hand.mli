open Base
open Consumables
module HandMap : Map.S with type key = Hand.t

val planets : Planet.t HandMap.t
val score : Planet.t -> Hand.t -> Effect.change list
val consume : Planet.t -> Hand.record -> Hand.record

val base_scores : Effect.t HandMap.t

val base_hands : (Hand.t * Hand.record) list

type evaluation = {
  category : Hand.t;
  scoring_cards : Card.t list;
}

type error = Invalid_card_count of int

val evaluate : Card.t list -> (evaluation, error) result

val is_high_card : Card.t list -> bool
val is_pair : Card.t list -> bool
val is_two_pair : Card.t list -> bool
val is_three_of_a_kind : Card.t list -> bool
val is_straight : Card.t list -> bool
val is_flush : Card.t list -> bool
val is_full_house : Card.t list -> bool
val is_four_of_a_kind : Card.t list -> bool
val is_straight_flush : Card.t list -> bool
val is_royal_flush : Card.t list -> bool
val is_five_of_a_kind : Card.t list -> bool
val is_flush_house : Card.t list -> bool
val is_flush_five : Card.t list -> bool
