type t = { chips : int; mult : int }

let create ~chips ~mult = { chips; mult }

type change = AddChips of int | AddMult of int | MultMult of int
