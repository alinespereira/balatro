type t = {
  evaluation: Poker.Hand.evaluation;
  cards: Card.t list;
}

let make cards =
  Poker.Hand.evaluate cards |> Result.map (fun evaluation -> { evaluation; cards })
;;

let hand { evaluation; _ } = evaluation.category
let scoring_cards { evaluation; _ } = evaluation.scoring_cards
let cards { cards; _ } = cards

let without debuffed play =
  let { evaluation; _ } = play in
  let new_scoring_cards =
    scoring_cards play |> List.filter (fun card -> not (debuffed card))
  in
  { play with evaluation = { evaluation with scoring_cards = new_scoring_cards } }
;;

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
