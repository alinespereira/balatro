type t = { cards: Card.t list }

let rec make play = { cards = play }

let score { cards } =
  match Poker.Hand.evaluate cards with
  | Ok evaluation ->
    let hand_score = Poker.Hand.base_score evaluation.category in
    evaluation.scoring_cards
    |> List.map (fun (card : Card.t) -> Card.rank_to_enum card.rank)
    |> List.map (fun value -> min 11 value)
    |> List.map (fun value -> Score.AddChips value)
    |> List.fold_left (fun acc value -> Score.apply value acc) hand_score
    |> Score.total
  | Error err -> 0
;;
