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

(** What one level up adds to a hand's score. *)
val bonus : t -> Score.change list
