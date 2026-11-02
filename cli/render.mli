(** Lays out the game state as text for the terminal. Every function here is pure: it
    builds a string and leaves the printing to [Main]. The names of things come from
    [Balatro_text.Label]; this module only decides where they go on the screen. *)

(** The hand on one line, each card under its 1-based position. *)
val hand : Balatro.Card.t list -> string

(** The jokers owned, one per line under their 1-based position, or an empty string
    when there are none. *)
val jokers : Balatro.Joker.t list -> string

(** The consumables owned, one per line under their 1-based position, or an empty
    string when there are none. *)
val consumables : Balatro.Consumables.Consumable.t list -> string

(** The money the player has, such as ["Dinheiro: $4"]. *)
val money : int -> string

(** The shop's offers, each under its 1-based position with its price, and the cost of
    the next reroll. *)
val shop : Balatro.Shop.t -> string

(** Ante, stage, the boss of the ante and what it does, chips against the target, the
    plays and discards left, and the money. *)
val status : Balatro.Run.t -> string

(** What a play made: the poker hand and the points it scored. *)
val play : Balatro.Play.t -> int -> string

(** What a beaten blind paid, part by part, and the total. *)
val payout : Balatro.Payout.t -> string
