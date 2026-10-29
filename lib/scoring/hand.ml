type record = {
  hand: Poker_hand.t;
  level: int;
  scoring_effect: Score.t;
}

let base_hands =
  Poker_hand.all
  |> List.rev
  |> List.map (fun hand ->
    hand, { hand; level = 1; scoring_effect = Poker_hand.base_score hand })
;;

let score planet hand =
  if Consumables.Planet.(equal (of_hand hand) planet) then
    Consumables.Planet.bonus planet
  else
    []
;;

let consume planet (hand : record) =
  match score planet hand.hand with
  | [] -> hand
  | changes ->
    let scoring_effect = Score.apply_all changes hand.scoring_effect in
    { hand with level = hand.level + 1; scoring_effect }
;;
