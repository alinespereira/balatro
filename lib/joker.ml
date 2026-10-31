type rarity =
  | Common
  | Uncommon
  | Rare
  | Legendary
[@@deriving show, eq]

type t =
  | Joker
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | JollyJoker
  | HalfJoker
  | Banner
  | AbstractJoker
  | Cavendish
[@@deriving show, eq, enumerate]

(* All the jokers so far are common in Balatro. *)
let rarity = function
  | Joker
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | JollyJoker
  | HalfJoker
  | Banner
  | AbstractJoker
  | Cavendish ->
    Common
;;

let cost = function
  | Joker -> 2
  | JollyJoker -> 3
  | AbstractJoker
  | Cavendish ->
    4
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | HalfJoker
  | Banner ->
    5
;;

let sell_value joker = max 1 (cost joker)

type context = {
  play: Play.t;
  discards_left: int;
  jokers: t list;
}

let scoring_with_suit ~suit play =
  play
  |> Play.scoring_cards
  |> List.filter (fun { Card.suit = s; _ } -> s = suit)
  |> List.length
;;

let changes ctx joker =
  let { play; discards_left; jokers } = ctx in
  match joker with
  | Joker -> [Score.AddMult 4.0]
  | JollyJoker ->
    let { Play.evaluation; _ } = play in
    let { Poker.Hand.category; _ } = evaluation in
    if category = Poker.Hand.Pair then
      [Score.AddMult 8.0]
    else
      []
  | AbstractJoker ->
    let { jokers; _ } = ctx in
    [Score.AddMult (3.0 *. float_of_int (List.length jokers))]
  | Cavendish -> [Score.TimesMult 3.0]
  | GreedyJoker ->
    let scoring_diamonds = scoring_with_suit ~suit:Card.Diamonds play in
    [Score.AddMult (3.0 *. float_of_int scoring_diamonds)]
  | LustyJoker ->
    let scoring_hearts = scoring_with_suit ~suit:Card.Hearts play in
    [Score.AddMult (3.0 *. float_of_int scoring_hearts)]
  | WrathfulJoker ->
    let scoring_spades = scoring_with_suit ~suit:Card.Spades play in
    [Score.AddMult (3.0 *. float_of_int scoring_spades)]
  | GluttonousJoker ->
    let scoring_clubs = scoring_with_suit ~suit:Card.Clubs play in
    [Score.AddMult (3.0 *. float_of_int scoring_clubs)]
  | HalfJoker ->
    let { play; _ } = ctx in
    let n = play |> Play.cards |> List.length in
    if n <= 3 then
      [Score.AddMult 20.0]
    else
      []
  | Banner ->
    let { discards_left; _ } = ctx in
    [Score.AddChips (30 * discards_left)]
;;

let all_changes ctx = ctx.jokers |> List.concat_map (changes ctx)
