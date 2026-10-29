module HandMap = Map.Make (Hand)

type entry = {
  level: int;
  score: Score.t;
}

type t = entry HandMap.t

let initial =
  Hand.all
  |> List.map (fun hand -> hand, { level = 1; score = Hand.base_score hand })
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
           level = entry.level + 1;
           score = Score.apply_all (Consumables.Planet.bonus planet) entry.score;
         }
       else
         entry)
    levels
;;
