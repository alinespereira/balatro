type t = Poker.Hand.evaluation

let make = Poker.Hand.evaluate
let hand (play : t) = play.category
let scoring_cards (play : t) = play.scoring_cards

let card_chips (card : Card.t) =
  match card.rank with
  | Card.Jack
  | Card.Queen
  | Card.King ->
    10
  | Card.Ace -> 11
  | rank -> Card.rank_to_enum rank
;;

let score levels play =
  let base = (Poker.Levels.get (hand play) levels).score in
  let changes =
    scoring_cards play |> List.map (fun card -> Score.AddChips (card_chips card))
  in
  Score.apply_all changes base
;;
