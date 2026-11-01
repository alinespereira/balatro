(** The shop opened between two blinds: a few offers to buy and a reroll that replaces
    them for a price that grows every time it is used. *)

type item =
  | JokerItem of Joker.t
  | PlanetItem of Consumables.Planet.t
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

(** What an item costs: [Joker.cost] for a joker, $3 for a planet. *)
val price : item -> int

(** Every item the shop can offer: each joker and each planet once. *)
val all_items : item list

(** A shop with [offer_count] offers drawn from [all_items], without repeats, and the
    reroll at [base_reroll_cost]. *)
val create : Random.State.t -> t

(** Takes the offer at the given position out of the shop and returns it. Paying for it
    is up to the caller. *)
val take : int -> t -> (item * t, error) result

(** Replaces the offers with [offer_count] new ones and raises the reroll cost by $1.
    Paying for it is up to the caller, with the cost from before the reroll. *)
val reroll : Random.State.t -> t -> t
