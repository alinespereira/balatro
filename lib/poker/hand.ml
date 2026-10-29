type t =
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  | FiveOfAKind
  | FlushHouse
  | FlushFive
[@@deriving show, eq, ord, enum, enumerate]

let base_score = function
  | HighCard -> Score.create ~chips:5 ~mult:1.
  | Pair -> Score.create ~chips:10 ~mult:2.
  | TwoPair -> Score.create ~chips:20 ~mult:2.
  | ThreeOfAKind -> Score.create ~chips:30 ~mult:3.
  | Straight -> Score.create ~chips:30 ~mult:4.
  | Flush -> Score.create ~chips:35 ~mult:4.
  | FullHouse -> Score.create ~chips:40 ~mult:4.
  | FourOfAKind -> Score.create ~chips:60 ~mult:7.
  | StraightFlush
  | RoyalFlush ->
    Score.create ~chips:100 ~mult:8.
  | FiveOfAKind -> Score.create ~chips:120 ~mult:12.
  | FlushHouse -> Score.create ~chips:140 ~mult:14.
  | FlushFive -> Score.create ~chips:160 ~mult:16.
;;

let planet : t -> Consumables.Planet.t = function
  | HighCard -> Pluto
  | Pair -> Mercury
  | TwoPair -> Uranus
  | ThreeOfAKind -> Venus
  | Straight -> Saturn
  | Flush -> Jupiter
  | FullHouse -> Earth
  | FourOfAKind -> Mars
  | StraightFlush -> Neptune
  | RoyalFlush -> Neptune
  | FiveOfAKind -> PlanetX
  | FlushHouse -> Ceres
  | FlushFive -> Eris
;;

type evaluation = {
  category: t;
  scoring_cards: Card.t list;
}

type error = InvalidCardCount of int

let group_by_rank = Util.groups ~key:(fun (card : Card.t) -> card.rank)
let group_by_suit = Util.groups ~key:(fun (card : Card.t) -> card.suit)
let all_same_rank cards = group_by_rank cards |> List.length = 1
let all_same_suit cards = group_by_suit cards |> List.length = 1

let rank_counts cards =
  let groups = group_by_rank cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort (Fun.flip Int.compare)
;;

let suit_counts cards =
  let groups = group_by_suit cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort (Fun.flip Int.compare)
;;

let count_groups ~size ~fn cards =
  cards |> fn |> List.filter (fun count -> count = size) |> List.length
;;

let count_rank_pairs cards = count_groups ~size:2 ~fn:rank_counts cards
let count_rank_triplets cards = count_groups ~size:3 ~fn:rank_counts cards
let count_rank_quadruplets cards = count_groups ~size:4 ~fn:rank_counts cards
let count_suit_pairs cards = count_groups ~size:2 ~fn:suit_counts cards
let count_suit_triplets cards = count_groups ~size:3 ~fn:suit_counts cards

let is_in_sequence cards =
  let eval ~enum_transform =
    let ranks =
      List.map (fun (card : Card.t) -> Card.rank_to_enum card.rank) cards
      |> List.map enum_transform
      |> List.sort Int.compare
    in
    let init = ranks |> List.rev |> List.tl |> List.rev in
    let tail = List.tl ranks in
    List.combine tail init
    |> List.map (fun (snd, fst) -> snd - fst)
    |> List.for_all (fun diff -> diff = 1)
  in
  let mod_transform value = value mod List.length Card.all_of_rank in
  eval ~enum_transform:Fun.id || eval ~enum_transform:mod_transform
;;

let evaluate _ = failwith "Not implemented"
let is_three_of_a_kind cards = count_rank_triplets cards = 1 && count_rank_pairs cards = 0

let is_flush_five cards =
  List.length cards = 5 && all_same_suit cards && all_same_rank cards
;;

let is_flush_house cards =
  count_rank_pairs cards = 1 && count_rank_triplets cards = 1 && all_same_suit cards
;;

let is_five_of_a_kind cards =
  List.length cards = 5 && all_same_rank cards && not (all_same_suit cards)
;;

let is_royal_flush _ = failwith "Not implemented"

let is_straight_flush cards =
  List.length cards = 5 && all_same_suit cards && is_in_sequence cards
;;

let is_four_of_a_kind cards = count_rank_quadruplets cards = 1

let is_full_house cards =
  count_rank_pairs cards = 1 && count_rank_triplets cards = 1 && not (all_same_suit cards)
;;

let is_flush cards =
  List.length cards = 5
  && all_same_suit cards
  && (not (is_in_sequence cards))
  && (not (is_flush_house cards))
  && not (is_flush_five cards)
;;

let is_straight cards =
  List.length cards = 5 && (not (all_same_suit cards)) && is_in_sequence cards
;;

let is_two_pair cards = count_rank_pairs cards = 2 && not (all_same_suit cards)

let is_pair cards =
  count_rank_pairs cards = 1 && count_rank_triplets cards = 0 && not (is_flush cards)
;;

let is_high_card cards =
  (List.for_all (fun count -> count = 1) (rank_counts cards)
   && ((not (all_same_suit cards)) || List.length cards < 5)
   && ((not (is_in_sequence cards)) || List.length cards < 5))
  || List.length cards = 1
;;

let get_highest_card cards =
  cards
  |> List.sort (fun (card_1 : Card.t) (card_2 : Card.t) ->
    Card.compare_rank card_2.rank card_1.rank)
  |> List.hd
;;
