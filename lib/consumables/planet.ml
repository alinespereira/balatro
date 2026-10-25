open Hand
open Scoring_effect

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

let score planet hand =
  match (planet, hand) with
  | Pluto, High_card -> [ AddChips 10; AddMult 1 ]
  | Mercury, Pair -> [ AddChips 15; AddMult 1 ]
  | Uranus, Two_pair -> [ AddChips 20; AddMult 1 ]
  | Venus, Three_of_a_kind -> [ AddChips 20; AddMult 2 ]
  | Saturn, Straight -> [ AddChips 30; AddMult 3 ]
  | Jupiter, Flush -> [ AddChips 15; AddMult 2 ]
  | Earth, Full_house -> [ AddChips 25; AddMult 2 ]
  | Mars, Four_of_a_kind -> [ AddChips 30; AddMult 3 ]
  | Neptune, (Straight_flush | Royal_flush) -> [ AddChips 40; AddMult 4 ]
  | _ -> []

let consume planet (hand : Hand.record) =
  match score planet hand.hand with
  | [] -> hand
  | changes ->
      let scoring_effect =
        List.fold_left
          (fun current change -> apply change current)
          hand.scoring_effect changes
      in
      { hand with level = hand.level + 1; scoring_effect }
