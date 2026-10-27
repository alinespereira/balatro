open Scoring

type t =
  | Pluto
  | Mercury
  | Uranus
  | Venus
  | Saturn
  | Jupiter
  | Earth
  | Mars
  | Neptune

val score : t -> Hand.t -> Effect.change list

val consume : t -> Hand.record -> Hand.record
