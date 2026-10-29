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

let count_groups ~size ~fn cards =
  cards |> fn |> List.filter (fun count -> count = size) |> List.length
;;

let count_rank_pairs cards = count_groups ~size:2 ~fn:rank_counts cards
let count_rank_triplets cards = count_groups ~size:3 ~fn:rank_counts cards
let count_rank_quadruplets cards = count_groups ~size:4 ~fn:rank_counts cards

(* Balatro plays from 1 to 5 cards. *)
let is_valid_size cards =
  let count = List.length cards in
  count >= 1 && count <= 5
;;

(* A flush needs all five cards of one suit: four hearts are not a flush. *)
let is_five_suited cards = List.length cards = 5 && all_same_suit cards

let rec is_consecutive = function
  | first :: (second :: _ as rest) -> second = first + 1 && is_consecutive rest
  | _ -> true
;;

(* The ace is the highest rank, but it also counts as 1 in A-2-3-4-5. *)
let is_five_in_sequence cards =
  let ace = Card.rank_to_enum Card.Ace in
  let ranks ~ace_value =
    cards
    |> List.map (fun (card : Card.t) -> Card.rank_to_enum card.rank)
    |> List.map (fun value -> if value = ace then ace_value else value)
    |> List.sort Int.compare
  in
  List.length cards = 5
  && (is_consecutive (ranks ~ace_value:ace) || is_consecutive (ranks ~ace_value:1))
;;

let has_royal_ranks cards =
  let ranks = List.map (fun (card : Card.t) -> card.rank) cards in
  List.sort Card.compare_rank ranks = Card.[Ten; Jack; Queen; King; Ace]
;;

(* Each [is_*] check answers whether the category is exactly the hand's category, so
   for any valid hand exactly one of them is true. A weaker category rules out the
   stronger ones that could contain it: three of a kind in five cards of one suit is a
   flush, for example. *)

let is_flush_five cards =
  List.length cards = 5 && all_same_suit cards && all_same_rank cards
;;

let is_flush_house cards =
  is_valid_size cards
  && count_rank_pairs cards = 1
  && count_rank_triplets cards = 1
  && all_same_suit cards
;;

let is_five_of_a_kind cards =
  List.length cards = 5 && all_same_rank cards && not (all_same_suit cards)
;;

let is_royal_flush cards = is_five_suited cards && has_royal_ranks cards

let is_straight_flush cards =
  is_five_suited cards && is_five_in_sequence cards && not (has_royal_ranks cards)
;;

let is_four_of_a_kind cards = is_valid_size cards && count_rank_quadruplets cards = 1

let is_full_house cards =
  is_valid_size cards
  && count_rank_pairs cards = 1
  && count_rank_triplets cards = 1
  && not (all_same_suit cards)
;;

let is_flush cards =
  is_five_suited cards
  && (not (is_five_in_sequence cards))
  && count_rank_quadruplets cards = 0
  && (not (is_flush_house cards))
  && not (is_flush_five cards)
;;

let is_straight cards = is_five_in_sequence cards && not (all_same_suit cards)

let is_three_of_a_kind cards =
  is_valid_size cards
  && count_rank_triplets cards = 1
  && count_rank_pairs cards = 0
  && not (is_five_suited cards)
;;

let is_two_pair cards =
  is_valid_size cards && count_rank_pairs cards = 2 && not (is_five_suited cards)
;;

let is_pair cards =
  is_valid_size cards
  && count_rank_pairs cards = 1
  && count_rank_triplets cards = 0
  && not (is_five_suited cards)
;;

let is_high_card cards =
  is_valid_size cards
  && List.for_all (fun count -> count = 1) (rank_counts cards)
  && (not (is_five_suited cards))
  && not (is_five_in_sequence cards)
;;

let get_highest_card cards =
  cards
  |> List.sort (fun (card_1 : Card.t) (card_2 : Card.t) ->
    Card.compare_rank card_2.rank card_1.rank)
  |> List.hd
;;

(* Only the cards that make the category score: the pair in a pair, the highest card in
   a high card, and every card in hands that use all five. *)
let scoring_cards category cards =
  let repeated_ranks =
    group_by_rank cards
    |> List.filter (fun (_, group) -> List.length group > 1)
    |> List.map fst
  in
  match category with
  | HighCard -> [get_highest_card cards]
  | Pair
  | TwoPair
  | ThreeOfAKind
  | FourOfAKind ->
    List.filter (fun (card : Card.t) -> List.mem card.rank repeated_ranks) cards
  | Straight
  | Flush
  | FullHouse
  | StraightFlush
  | RoyalFlush
  | FiveOfAKind
  | FlushHouse
  | FlushFive ->
    cards
;;

let detectors =
  [
    FlushFive, is_flush_five;
    FlushHouse, is_flush_house;
    FiveOfAKind, is_five_of_a_kind;
    RoyalFlush, is_royal_flush;
    StraightFlush, is_straight_flush;
    FourOfAKind, is_four_of_a_kind;
    FullHouse, is_full_house;
    Flush, is_flush;
    Straight, is_straight;
    ThreeOfAKind, is_three_of_a_kind;
    TwoPair, is_two_pair;
    Pair, is_pair;
    HighCard, is_high_card;
  ]
;;

let evaluate cards =
  if not (is_valid_size cards) then
    Error (InvalidCardCount (List.length cards))
  else (
    (* Safe: a valid hand always matches exactly one detector. *)
    let category, _ = List.find (fun (_, detect) -> detect cards) detectors in
    Ok { category; scoring_cards = scoring_cards category cards }
  )
;;
