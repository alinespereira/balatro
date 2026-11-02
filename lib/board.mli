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
  | NoConsumableSlot (** Every consumable slot is taken. *)
  | NoConsumableAt of int (** There is no consumable at that position (0-based). *)
  | WrongSelection of {
      min: int;
      max: int;
      got: int;
    } (** A tarot needs from [min] to [max] cards, but [got] were selected. *)
[@@deriving show]

(** At most this many cards can be selected to play or discard. *)
val max_selected : int

(** A board with a freshly shuffled deck and an empty hand. *)
val create : Random.State.t -> Deck.color -> t

(** Puts every card back in the deck (from the hand and the discards) and shuffles
    it, for the next blind. Jokers, consumables and slots stay as they are. *)
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

(** The consumables owned, left to right in the order they were added. *)
val consumables : t -> Consumables.Consumable.t list

(** Adds the consumable to the right of the others, if a slot is free. *)
val add_consumable : Consumables.Consumable.t -> t -> (t, error) result

(** Takes out the consumable at the given position (0-based) and returns it, so it can
    be used. *)
val remove_consumable : int -> t -> (Consumables.Consumable.t * t, error) result

(** Uses the tarot on the cards at the given positions of the hand (0-based), which
    may come in any order. The number of positions must be within
    [Consumables.Tarot.selection], otherwise [WrongSelection]; a tarot that needs no
    cards takes [[]] and leaves the board as it is. Positions are checked as in [play]:
    [InvalidIndex] and [DuplicateIndex].

    The tarot gets the selected cards in the order they are in the hand. A changed card
    stays at its place in the hand, and keeps the change for the rest of the run. A
    destroyed card leaves the hand and the game: it does not go to the discards, so it
    never comes back to the deck. *)
val use_tarot : Consumables.Tarot.t -> int list -> t -> (t, error) result

(** Played and discarded cards, most recent first. *)
val discarded : t -> Card.t list

(** Draws from the deck until the hand is full or the deck is empty. *)
val refill : t -> t

(** Plays the cards at the given positions of the hand (0-based). *)
val play : int list -> t -> (Play.t * t, error) result

(** Discards the cards at the given positions of the hand (0-based). *)
val discard : int list -> t -> (t, error) result
