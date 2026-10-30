(** Chips and mult, the two numbers a hand multiplies together to score. *)
type t = {
  chips: int;
  mult: float;
}
[@@deriving show, eq]

(** A change to a score. Order matters: [AddMult] before [TimesMult] scores
    differently than the other way around. *)
type change =
  | AddChips of int
  | AddMult of float
  | TimesMult of float
[@@deriving show, eq]

val create : chips:int -> mult:float -> t
val apply : change -> t -> t

(** Applies the changes from left to right. *)
val apply_all : change list -> t -> t

(** [chips × mult]. *)
val total : t -> int
