(** A whole run: a sequence of blinds that get harder as the run goes on.

    Every ante has three blinds, played in order: [Small], [Big] and [Boss]. Beating
    the boss blind of [final_ante] wins the run; losing any blind loses it. Hand levels
    carry over from one blind to the next, but every blind starts with a new board and
    a freshly shuffled deck. Money also carries over: the run starts with
    [starting_money] and every beaten blind pays a [Payout.t]. *)

(** Which of the three blinds of an ante is being played. *)
type stage =
  | Small
  | Big
  | Boss
[@@deriving show, eq]

(** Where the run is between two blinds. After a blind is beaten, [cash_out] pays it
    and opens the shop; [leave_shop] closes it and starts the next blind. While
    shopping, the run keeps the board of the beaten blind, so jokers bought or sold
    carry over to the next one. *)
type phase =
  | Playing
  | Shopping of {
      shop: Shop.t;
      board: Board.t;
    }

(** The current ante and stage, the blind being played, the hand levels, the money and
    the phase. *)
type t = private {
  ante: int;
  stage: stage;
  blind: Blind.t;
  levels: Poker.Levels.t;
  money: int;
  phase: phase;
}

(** [InProgress] also covers a blind that was just beaten and is waiting for
    [cash_out], and the shop; check [Blind.status (blind run)] and [phase] to tell
    them apart. *)
type status =
  | InProgress
  | Won
  | Lost
[@@deriving show, eq]

type error =
  | BlindError of Blind.error (** The current blind refused the play or the discard. *)
  | BlindNotOver (** [cash_out] was called before the current blind was beaten. *)
  | RunOver (** The run is already won or lost. *)
  | InShop (** Only the shop commands work while shopping. *)
  | NotInShop (** The shop commands only work while shopping. *)
  | NotEnoughMoney of int (** The price that could not be paid. *)
  | ShopError of Shop.error (** The shop refused: no offer at that position. *)
  | BoardError of Board.error (** The board refused: no joker slot, or no joker there. *)
[@@deriving show]

(** The last ante: beating its boss blind wins the run. *)
val final_ante : int

(** Points needed to beat the blind at [stage] of [ante]. Grows with the ante, and
    within an ante [Small < Big < Boss]. *)
val target : ante:int -> stage -> int

(** What a run starts with: $4. *)
val starting_money : int

(** What beating the blind at [stage] pays before plays and interest: $3 for [Small],
    $4 for [Big] and $5 for [Boss]. *)
val reward : stage -> int

(** A run at the small blind of ante 1, every hand at level 1, with [starting_money]. *)
val create : Random.State.t -> Deck.color -> t

val ante : t -> int
val stage : t -> stage
val blind : t -> Blind.t
val levels : t -> Poker.Levels.t
val money : t -> int
val phase : t -> phase

(** The board of the blind being played or, while shopping, the one that carries over
    to the next blind. *)
val board : t -> Board.t

val status : t -> status

(** Plays the cards at the given positions of the hand in the current blind, scored
    with the run's hand levels. Returns the play, its points and the new run. *)
val play : int list -> t -> (Play.t * int * t, error) result

(** Discards the cards at the given positions of the hand in the current blind. *)
val discard : int list -> t -> (t, error) result

(** Pays the blind that was just beaten and opens the shop. The payout is added to the
    money and returned, so it can be shown. Beating the boss blind of [final_ante] wins
    the run instead, and there is nothing to cash out. *)
val cash_out : Random.State.t -> t -> (Payout.t * t, error) result

(** Closes the shop and moves on to the next blind: [Small] to [Big], [Big] to [Boss],
    and [Boss] to the [Small] blind of the next ante. The new blind gets the shop's
    board, reset and shuffled with the given state. *)
val leave_shop : Random.State.t -> t -> (t, error) result

(** Buys the offer at the given position (0-based) if there is money for it. A joker
    goes to the board; a planet is used right away. *)
val buy : int -> t -> (t, error) result

(** Sells the joker at the given position (0-based) for [Joker.sell_value]. *)
val sell_joker : int -> t -> (t, error) result

(** Pays [reroll_cost] and replaces the shop's offers. *)
val reroll : Random.State.t -> t -> (t, error) result

(** Levels up the hands of the planet. The new levels count from the next play on. *)
val use_planet : Consumables.Planet.t -> t -> t
