(** Turns the game state into text for the terminal. Every function here is pure: it
    builds a string and leaves the printing to [Main]. *)

(** A card in two or three characters, such as ["K♥"] or ["10♠"]. *)
val card : Balatro.Card.t -> string

(** The hand on one line, each card under its 1-based position. *)
val hand : Balatro.Card.t list -> string

(** The name the player sees, in Portuguese. *)
val joker_name : Balatro.Joker.t -> string

(** What the joker does, in Portuguese. *)
val joker_description : Balatro.Joker.t -> string

(** The jokers owned, one per line under their 1-based position, or an empty string
    when there are none. *)
val jokers : Balatro.Joker.t list -> string

(** The name of a planet, in Portuguese. *)
val planet_name : Balatro.Consumables.Planet.t -> string

(** The shop's offers, each under its 1-based position with its price, and the cost of
    the next reroll. *)
val shop : Balatro.Shop.t -> string

(** Ante, stage, chips against the target, the plays and discards left, and the money. *)
val status : Balatro.Run.t -> string

(** What a play made: the poker hand and the points it scored. *)
val play : Balatro.Play.t -> int -> string

(** What a beaten blind paid, part by part, and the total. *)
val payout : Balatro.Payout.t -> string

(** A message for an error returned by any function of [Run]. *)
val error : Balatro.Run.error -> string
