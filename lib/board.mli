(** The table during a round: the deck, the cards in hand and the ones already used.
    Every operation returns a new board; the old one is left untouched. *)
type t = private {
  deck: Deck.t;
  hand: Card.t list;
  discarded: Card.t list;
  jokers: Joker.t list;
  consumables: Consumables.Consumable.t list;
  hand_size: int;
  joker_slots: int;
  consumable_slots: int;
}

(** Why a selection of cards could not be played or discarded. *)
type error =
  | NoCards
  | TooManyCards of int
  | InvalidIndex of int
  | DuplicateIndex of int
  | InvalidPlay of Poker.Hand.error
  | NoJokerSlot (** Every joker slot is taken. *)
  | NoJokerAt of int (** There is no joker at that position (0-based). *)
[@@deriving show]

(** At most this many cards can be selected to play or discard. *)
val max_selected : int

(** A board with a freshly shuffled deck and an empty hand. *)
val create : Random.State.t -> Deck.color -> t

val reset : Random.State.t -> t -> t
val deck : t -> Deck.t
val hand : t -> Card.t list

(** The jokers owned, in the order they score: left to right. *)
val jokers : t -> Joker.t list

(** Adds the joker to the right of the others, if a slot is free. *)
val add_joker : Joker.t -> t -> (t, error) result

(** Takes out the joker at the given position (0-based) and returns it, so it can be
    sold. *)
val remove_joker : int -> t -> (Joker.t * t, error) result

(** Played and discarded cards, most recent first. *)
val discarded : t -> Card.t list

(** Draws from the deck until the hand is full or the deck is empty. *)
val refill : t -> t

(** Plays the cards at the given positions of the hand (0-based). *)
val play : int list -> t -> (Play.t * t, error) result

(** Discards the cards at the given positions of the hand (0-based). *)
val discard : int list -> t -> (t, error) result
