type enhancement =
  | Bonus
  | Mult
  | Wild
  | Glass
  | Steel
  | Stone
  | Gold
  | Lucky

type edition =
  | Foil
  | Holographic
  | Polychrome
  | Negative

type seal =
  | Red
  | Blue
  | Gold
  | Purple

type t = {
  enhancement: enhancement option;
  edition: edition option;
  seal: seal option;
}

val none : t
