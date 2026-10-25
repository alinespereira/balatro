type t = { chips : int; mult : int }
type enhancement = Bonus | Mult | Wild | Glass | Steel | Stone | Gold | Lucky
type edition = Foil | Holographic | Polychrome | Negative
type seal = Red | Blue | Gold | Purple

type decorations = {
  enhancement : enhancement option;
  edition : edition option;
  seal : seal option;
}

let no_decorations = { enhancement = None; edition = None; seal = None }
let create chips mult = { chips; mult }

type scoring_change = AddChips of int | AddMult of int | MultMult of int

let apply change eff =
  match change with
  | AddChips n -> { eff with chips = eff.chips + n }
  | AddMult n -> { eff with mult = eff.mult + n }
  | MultMult n -> { eff with mult = eff.mult * n }
