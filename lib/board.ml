type error =
  | NoCards
  | TooManyCards of int
  | InvalidIndex of int
  | DuplicateIndex of int
  | InvalidPlay of Poker.Hand.error
  | NoJokerSlot
  | NoJokerAt of int
[@@deriving show]

type t = {
  deck: Deck.t;
  hand: Card.t list;
  discarded: Card.t list;
  jokers: Joker.t list;
  consumables: Consumables.Consumable.t list;
  hand_size: int;
  joker_slots: int;
  consumable_slots: int;
}

let default_hand_size = 8
let default_joker_slots = 5
let default_consumable_slots = 2
let max_selected = 5
let ( let* ) = Result.bind
let deck { deck; _ } = deck
let hand { hand; _ } = hand
let discarded { discarded; _ } = discarded
let jokers { jokers; _ } = jokers

let add_joker joker board =
  let { jokers; joker_slots; _ } = board in
  if List.length jokers < joker_slots then
    Ok { board with jokers = joker :: jokers }
  else
    Error NoJokerSlot
;;

let remove_joker idx board =
  let { jokers; _ } = board in
  match List.nth_opt jokers idx with
  | Some joker when idx >= 0 ->
    Ok (joker, { board with jokers = List.filteri (fun i _ -> i != idx) jokers })
  | _ -> Error (NoJokerAt idx)
;;

let create rng color =
  let deck = Deck.create color |> Deck.shuffle rng in
  {
    deck;
    hand = [];
    discarded = [];
    jokers = [];
    consumables = [];
    hand_size = default_hand_size;
    joker_slots = default_joker_slots;
    consumable_slots = default_consumable_slots;
  }
;;

let reset rng board =
  let { deck; hand; discarded; _ } = board in
  let cards = Deck.cards deck @ hand @ discarded in
  let deck = Deck.set_cards cards deck in
  let deck = Deck.shuffle rng deck in
  { board with deck; hand = []; discarded = [] }
;;

(* The deck is shuffled once in [create], so drawing is just taking cards from the top.
   [Util.split_at] returns fewer cards when the deck runs out, so an empty deck is not
   an error: the hand simply stays smaller. *)
let refill board =
  let missing = board.hand_size - List.length board.hand in
  let drawn, rest = Util.split_at missing (Deck.cards board.deck) in
  { board with deck = Deck.set_cards rest board.deck; hand = board.hand @ drawn }
;;

let validate_indices size indices =
  let rec loop seen = function
    | [] -> Ok ()
    | idx :: rest ->
      if idx < 0 || idx >= size then
        Error (InvalidIndex idx)
      else if List.mem idx seen then
        Error (DuplicateIndex idx)
      else
        loop (idx :: seen) rest
  in
  match List.length indices with
  | 0 -> Error NoCards
  | count when count > max_selected -> Error (TooManyCards count)
  | _ -> loop [] indices
;;

(* Splits the hand into the selected cards and the ones that stay, keeping the order
   they have in the hand. *)
let select indices hand =
  let* () = validate_indices (List.length hand) indices in
  let chosen, kept =
    hand
    |> List.mapi (fun pos card -> pos, card)
    |> List.partition (fun (pos, _) -> List.mem pos indices)
  in
  Ok (List.map snd chosen, List.map snd kept)
;;

let play indices board =
  let* selected, kept = select indices board.hand in
  let* play = Play.make selected |> Result.map_error (fun e -> InvalidPlay e) in
  Ok (play, { board with hand = kept; discarded = selected @ board.discarded })
;;

let discard indices board =
  let* selected, kept = select indices board.hand in
  Ok { board with hand = kept; discarded = selected @ board.discarded }
;;
