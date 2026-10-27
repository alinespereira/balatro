open Scoring

type t =
  | High_card
  | Pair
  | Two_pair
  | Three_of_a_kind
  | Straight
  | Flush
  | Full_house
  | Four_of_a_kind
  | Straight_flush
  | Royal_flush
  | Five_of_a_kind
  | Flush_house
  | Flush_five

val is_full_house : Card.t list -> bool
(** Whether exactly five cards contain three of one rank and two of another.
    Suits and decorations are ignored. *)

type record = { hand : t; level : int; scoring_effect : Effect.t }

val base_hands : (t * record) list
