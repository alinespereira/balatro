(** What the player can do to a run, whatever the interface: the terminal parses it
    from a line, a graphical interface would build it from a click. [apply] runs it
    and says what happened, so every interface follows the same flow after a play. *)

(** Positions are 0-based, as [Run] expects. *)
type t =
  | Play of int list (** Plays the cards at those positions of the hand. *)
  | Discard of int list (** Discards the cards at those positions of the hand. *)
  | Buy of int (** Buys the shop offer at that position. *)
  | Sell of int (** Sells the joker at that position. *)
  | Use of int * int list
  (** Uses the consumable at the first position on the cards at the others. *)
  | Reroll (** Replaces the shop offers. *)
  | Leave (** Leaves the shop for the next blind. *)
[@@deriving show, eq]

(** What an action caused, for the interface to show. The plays carry the play and its
    points, as [Run.play] returns them. *)
type outcome =
  | Played of Play.t * int (** The play did not beat the blind; it goes on. *)
  | BlindWon of Play.t * int * Payout.t
  (** The play beat the blind, which was paid with [Run.cash_out]: the run is now in
          the shop. *)
  | RunWon of Play.t * int (** The play beat the boss blind of [Run.final_ante]. *)
  | RunLost of Play.t * int (** That was the last play and the blind was not beaten. *)
  | Done (** A discard, a purchase, a sale, a use, a reroll or leaving the shop. *)

(** Runs the action on the run, using the random state for [Run.cash_out], [Run.reroll]
    and [Run.leave_shop]. Returns what happened and the new run, or the error of the
    [Run] function that refused it; then the run stays as it was. *)
val apply : Random.State.t -> t -> Run.t -> (outcome * Run.t, Run.error) result
