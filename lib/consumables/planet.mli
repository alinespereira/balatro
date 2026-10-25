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

val score : t -> Hand.t -> Scoring_effect.scoring_change list

val consume : t -> Hand.record -> Hand.record
