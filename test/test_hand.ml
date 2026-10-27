open Balatro
open Balatro.Base

let () =
  let suit = List.hd Card.all_suits in
  let first = List.nth Card.all_ranks 0 in
  let second = List.nth Card.all_ranks 1 in
  let third = List.nth Card.all_ranks 2 in
  let cards ranks = List.map (Card.create suit) ranks in
  let check expected ranks =
    assert (Scoring.Hand.is_full_house (cards ranks) = expected)
  in
  check true [ first; second; first; second; first ];
  check true [ second; first; first; second; first ];
  check false [ first; first; first; first; second ];
  check false [ first; first; first; first; first ];
  check false [ first; first; second; second; third ];
  check false [ first; first; first; second; third ];
  check false [];
  check false [ first; first; second; second ];
  check false [ first; first; first; second; second; second ]
