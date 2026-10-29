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

let of_hand : Poker_hand.t -> t = function
  | HighCard -> Pluto
  | Pair -> Mercury
  | TwoPair -> Uranus
  | ThreeOfAKind -> Venus
  | Straight -> Saturn
  | Flush -> Jupiter
  | FullHouse -> Earth
  | FourOfAKind -> Mars
  | StraightFlush
  | RoyalFlush ->
    Neptune
  | FiveOfAKind -> PlanetX
  | FlushHouse -> Ceres
  | FlushFive -> Eris
;;

let bonus : t -> Score.change list = function
  | Pluto -> [AddChips 10; AddMult 1.]
  | Mercury -> [AddChips 15; AddMult 1.]
  | Uranus -> [AddChips 20; AddMult 1.]
  | Venus -> [AddChips 20; AddMult 2.]
  | Saturn -> [AddChips 30; AddMult 3.]
  | Jupiter -> [AddChips 15; AddMult 2.]
  | Earth -> [AddChips 25; AddMult 2.]
  | Mars -> [AddChips 30; AddMult 3.]
  | Neptune -> [AddChips 40; AddMult 4.]
  | PlanetX -> [AddChips 35; AddMult 3.]
  | Ceres -> [AddChips 40; AddMult 4.]
  | Eris -> [AddChips 50; AddMult 3.]
;;
