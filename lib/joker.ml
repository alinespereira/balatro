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
  | GreenJoker of { mult: int }
  | RideTheBus of { mult: int }
  | Runner of { chips: int }
  | IceCream of { chips: int }
[@@deriving show, eq]

let all =
  [
    Joker;
    GreedyJoker;
    LustyJoker;
    WrathfulJoker;
    GluttonousJoker;
    JollyJoker;
    HalfJoker;
    Banner;
    AbstractJoker;
    Cavendish;
    GreenJoker { mult = 0 };
    RideTheBus { mult = 0 };
    Runner { chips = 0 };
    IceCream { chips = 100 };
  ]
;;

let fresh = function
  | GreenJoker _ -> GreenJoker { mult = 0 }
  | RideTheBus _ -> RideTheBus { mult = 0 }
  | Runner _ -> Runner { chips = 0 }
  | IceCream _ -> IceCream { chips = 100 }
  | ( Joker
    | GreedyJoker
    | LustyJoker
    | WrathfulJoker
    | GluttonousJoker
    | JollyJoker
    | HalfJoker
    | Banner
    | AbstractJoker
    | Cavendish ) as joker ->
    joker
;;

(* TODO(Aline): two jokers are the same kind when they are equal once their state is
   put back to the start. [fresh] already does that part. *)
let same_kind _a _b = failwith "TODO: Joker.same_kind"

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
  | Cavendish
  | GreenJoker _
  | RideTheBus _
  | Runner _
  | IceCream _ ->
    Common
;;

let cost = function
  | Joker -> 2
  | JollyJoker -> 3
  | AbstractJoker
  | Cavendish
  | GreenJoker _ ->
    4
  | GreedyJoker
  | LustyJoker
  | WrathfulJoker
  | GluttonousJoker
  | HalfJoker
  | Banner
  | Runner _
  | IceCream _ ->
    5
  | RideTheBus _ -> 6
;;

let sell_value joker = max 1 (cost joker / 2)

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
    if Poker.Hand.contains_pair category then
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
  (* TODO(Aline): the jokers with state score what their payload says. Remember that
     [Score.AddMult] takes a float: [float_of_int]. *)
  | GreenJoker _
  | RideTheBus _
  | Runner _
  | IceCream _ ->
    failwith "TODO: Joker.changes for the jokers with state"
;;

let all_changes ctx = ctx.jokers |> List.concat_map (changes ctx)

type event =
  | Played of Play.t
  | Discarded

(* TODO(Aline): one [match] on the event and the joker, returning the joker with its
   new state ([Some (GreenJoker { mult = mult + 1 })]), or [None] when it goes away.
   - GreenJoker: +1 per [Played], -1 per [Discarded], never below 0 ([max]).
   - RideTheBus: on [Played], back to 0 if a scoring card ([Play.scoring_cards]) is a
     Jack, Queen or King, +1 otherwise. A discard does not change it.
   - Runner: +15 on [Played] when the hand contains a straight: [Straight],
     [StraightFlush] or [RoyalFlush] ([Play.hand]).
   - IceCream: -5 on [Played]; at 0 or less it melts, so [None].
   - Every other joker, and every other event: [Some joker], unchanged. A catch-all
     [| _, joker -> Some joker] at the end is fine here. *)
let react _event _joker = failwith "TODO: Joker.react"
