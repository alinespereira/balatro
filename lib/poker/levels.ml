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

let record_play hand levels =
  HandMap.update
    hand
    (Option.map (fun { level; score; played } -> { level; score; played = played + 1 }))
    levels
;;
