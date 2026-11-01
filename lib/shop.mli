(** The shop opened between two blinds: a few offers to buy and a reroll that replaces
    them for a price that grows every time it is used. *)

type item =
  | JokerItem of Joker.t
  | PlanetItem of Consumables.Planet.t
  | TarotItem of Consumables.Tarot.t
[@@deriving show, eq]

type t = private {
  offers: item list; (** What is for sale, left to right. *)
  reroll_cost: int; (** What the next reroll costs. *)
}

type error = NoOfferAt of int (** There is no offer at that position (0-based). *)
[@@deriving show]

(** How many offers a fresh shop or a reroll shows: 2. *)
val offer_count : int

(** What the first reroll of a shop costs: $5. Every reroll after it costs $1 more. *)
val base_reroll_cost : int

(** What an item costs: [Joker.cost] for a joker, $3 for a planet or a tarot. *)
val price : item -> int

(** Every item the shop can offer: each joker, each planet and each
    [Consumables.Tarot.implemented] tarot once. *)
val all_items : item list

(** What the shop can offer right now: every item of [all_items] except

    - the jokers in [jokers], the ones already owned: no joker shows up twice;
    - the planets of the secret hands ([Poker.Hand.is_secret]) that were never played,
      according to [levels]. *)
val available : jokers:Joker.t list -> levels:Poker.Levels.t -> item list

(** A shop with [offer_count] offers drawn from [from] (by default, [all_items]),
    without repeats, and the reroll at [base_reroll_cost]. If [from] has fewer items
    than [offer_count], the shop offers all of them. *)
val create : ?from:item list -> Random.State.t -> t

(** Takes the offer at the given position out of the shop and returns it. Paying for it
    is up to the caller. *)
val take : int -> t -> (item * t, error) result

(** Replaces the offers with [offer_count] new ones drawn from [from], like [create],
    and raises the reroll cost by $1. Paying for it is up to the caller, with the cost
    from before the reroll. *)
val reroll : ?from:item list -> Random.State.t -> t -> t
