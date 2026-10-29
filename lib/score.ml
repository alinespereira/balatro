type t = {
  chips: int;
  mult: float;
}
[@@deriving show, eq]

type change =
  | AddChips of int
  | AddMult of float
  | TimesMult of float
[@@deriving show, eq]

let create ~chips ~mult = { chips; mult }

let apply change score =
  match change with
  | AddChips n -> { score with chips = score.chips + n }
  | AddMult n -> { score with mult = score.mult +. n }
  | TimesMult n -> { score with mult = score.mult *. n }
;;

let apply_all changes score = List.fold_left (fun acc change -> apply change acc) score changes
let total { chips; mult } = Float.of_int chips *. mult
