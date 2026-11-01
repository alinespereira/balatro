module HandMap = Map.Make (Hand)

type entry = {
  level: int;
  score: Score.t;
  played: int;
}

type t = entry HandMap.t

let initial =
  Hand.all
  |> List.map (fun hand -> hand, { level = 1; score = Hand.base_score hand; played = 0 })
  |> List.to_seq
  |> HandMap.of_seq
;;

(* Safe: [initial] has every hand and nothing removes one. *)
let get hand levels = HandMap.find hand levels

let use_planet planet levels =
  HandMap.mapi
    (fun hand entry ->
       if Consumables.Planet.equal (Hand.planet hand) planet then
         {
           entry with
           level = entry.level + 1;
           score = Score.apply_all (Consumables.Planet.bonus planet) entry.score;
         }
       else
         entry)
    levels
;;

(* TODO: add 1 to [played] in the entry of [hand], and leave the others as they are.
   For now it changes nothing, so the run still works. *)
let record_play _hand levels = levels
