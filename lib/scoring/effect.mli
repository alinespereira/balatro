type t
type decorations

type enhancement
type edition
type seal

val no_decorations : decorations

val create: int -> int -> t

type change = AddChips of int | AddMult of int | MultMult of int
val apply: change -> t -> t