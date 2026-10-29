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
[@@deriving show, eq, ord]

let all =
  [
    HighCard;
    Pair;
    TwoPair;
    ThreeOfAKind;
    Straight;
    Flush;
    FullHouse;
    FourOfAKind;
    StraightFlush;
    RoyalFlush;
    FiveOfAKind;
    FlushHouse;
    FlushFive;
  ]
;;

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

let rank_counts cards =
  let groups = group_by_rank cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare
;;

let suit_counts cards =
  let groups = group_by_suit cards in
  let _, group = List.split groups in
  group |> List.map List.length |> List.sort Int.compare
;;

let evaluate _ = failwith "Not implemented"
let is_high_card cards = not (List.is_empty cards)
let is_pair cards = rank_counts cards = [1; 1; 1; 2]
let is_two_pair cards = rank_counts cards = [1; 2; 2]
let is_three_of_a_kind cards = rank_counts cards = [1; 1; 2]
let is_straight _ = failwith "Not implemented"
let is_flush _ = failwith "Not implemented"
let is_full_house cards = rank_counts cards = [2; 3]
let is_four_of_a_kind _ = failwith "Not implemented"
let is_straight_flush _ = failwith "Not implemented"
let is_royal_flush _ = failwith "Not implemented"
let is_five_of_a_kind _ = failwith "Not implemented"
let is_flush_house _ = failwith "Not implemented"
let is_flush_five _ = failwith "Not implemented"

let get_high_card cards =
  cards
  |> List.sort (fun (card_1 : Card.t) (card_2 : Card.t) ->
    Card.compare_suit card_2.suit card_1.suit)
  |> List.hd
;;
