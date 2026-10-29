open Balatro
open Balatro.Base

let () =
  let cards ranks = List.map (Card.create Card.Clubs) ranks in
  let check expected ranks =
    assert (Scoring.Hand.is_full_house (cards ranks) = expected)
  in
  check true Card.[Two; Three; Two; Three; Two];
  check true Card.[Three; Two; Two; Three; Two];
  check false Card.[Two; Two; Two; Two; Three];
  check false Card.[Two; Two; Two; Two; Two];
  check false Card.[Two; Two; Three; Three; Four];
  check false Card.[Two; Two; Two; Three; Four];
  check false [];
  check false Card.[Two; Two; Three; Three];
  check false Card.[Two; Two; Two; Three; Three; Three]
;;
