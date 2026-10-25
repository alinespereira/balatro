type t =
  | High_card
  | Pair
  | Two_pair
  | Three_of_a_kind
  | Straight
  | Flush
  | Full_house
  | Four_of_a_kind
  | Straight_flush
  | Royal_flush
  | Five_of_a_kind
  | Flush_house
  | Flush_five

type record = { hand : t; level : int; scoring_effect : Scoring_effect.t }

let base_scores =
  [
    (Flush_five, (160, 16));
    (Flush_house, (140, 14));
    (Five_of_a_kind, (120, 12));
    (Royal_flush, (100, 8));
    (Straight_flush, (100, 8));
    (Four_of_a_kind, (60, 7));
    (Full_house, (40, 4));
    (Flush, (35, 4));
    (Straight, (30, 4));
    (Three_of_a_kind, (30, 3));
    (Two_pair, (20, 2));
    (Pair, (10, 2));
    (High_card, (5, 1));
  ]

let make_hand (hand, (chips, mult)) =
  (hand, { hand; level = 1; scoring_effect = Scoring_effect.create chips mult })

let base_hands = List.map make_hand base_scores

let is_full_house cards =
  let add_rank counts card =
    let rank = Card.rank card in
    let count = Option.value (List.assoc_opt rank counts) ~default:0 in
    (rank, count + 1) :: List.remove_assoc rank counts
  in
  List.length cards = 5
  && cards |> List.fold_left add_rank [] |> List.map snd
     |> List.sort Int.compare = [ 2; 3 ]
