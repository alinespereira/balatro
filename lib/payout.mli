(** The money paid for beating a blind, split into its parts so it can be shown. *)
type t = private {
  blind: int; (** The reward of the blind itself. *)
  plays: int; (** $1 for every play left. *)
  interest: int; (** $1 for every $5 held, capped at [max_interest]. *)
}
[@@deriving show, eq]

val max_interest : int

(** [money] is what the player has before being paid, so the reward of this blind does
    not earn interest yet. *)
val make : reward:int -> plays_left:int -> money:int -> t

(** [blind + plays + interest]. *)
val total : t -> int
