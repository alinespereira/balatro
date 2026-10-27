type t = { chips : int; mult : int }

val create : chips:int -> mult:int -> t

type change = AddChips of int | AddMult of int | MultMult of int
