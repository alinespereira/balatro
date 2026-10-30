type stage =
  | Small
  | Big
  | Boss
[@@deriving show, eq]

type t = {
  ante: int;
  stage: stage;
  blind: Blind.t;
  levels: Poker.Levels.t;
}

type status =
  | InProgress
  | Won
  | Lost
[@@deriving show, eq]

type error =
  | BlindError of Blind.error
  | BlindNotOver
  | RunOver
[@@deriving show]

let final_ante = 8

module IntMap = Map.Make (Int)

let ante_base = [300; 800; 2000; 5000; 11000; 20000; 35000; 50000]

let ante_score =
  ante_base
  |> List.mapi (fun idx target -> idx + 1, Float.of_int target)
  |> List.to_seq
  |> IntMap.of_seq
;;

(* TODO: base points per ante times a factor per stage. *)
let target ~ante:_ _stage = failwith "TODO: Run.target"

(* TODO: build the first blind with Board.create and Blind.create. *)
let create _rng _color = failwith "TODO: Run.create"
let ante { ante; _ } = ante
let stage { stage; _ } = stage
let blind { blind; _ } = blind
let levels { levels; _ } = levels

(* TODO: derive it from Blind.status, the ante and the stage. *)
let status _run = failwith "TODO: Run.status"
let play _indices _run = failwith "TODO: Run.play"
let discard _indices _run = failwith "TODO: Run.discard"
let next_blind _rng _run = failwith "TODO: Run.next_blind"
let use_planet _planet _run = failwith "TODO: Run.use_planet"
