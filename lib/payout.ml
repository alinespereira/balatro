type t = {
  blind: int;
  plays: int;
  interest: int;
}
[@@deriving show, eq]

let max_interest = 5

let make ~reward ~plays_left ~money =
  let blind = reward in
  let plays = plays_left in
  let interest = min max_interest (money / 5) in
  { blind; plays; interest }
;;

let total { blind; plays; interest } = blind + plays + interest
