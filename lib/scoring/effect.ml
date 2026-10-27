open Base.Effect

let apply change eff =
  match change with
  | AddChips n -> { eff with chips = eff.chips + n }
  | AddMult n -> { eff with mult = eff.mult + n }
  | MultMult n -> { eff with mult = eff.mult * n }
