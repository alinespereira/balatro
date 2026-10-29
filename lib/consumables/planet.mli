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
  | PlanetX
  | Ceres
  | Eris
[@@deriving show, eq, ord]

(** The planet that levels up a hand. Straight flush and royal flush share
    Neptune. *)
val of_hand : Poker.Hand.t -> t

(** What one level up adds to a hand's score. *)
val bonus : t -> Score.change list
