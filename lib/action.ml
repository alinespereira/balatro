type t =
  | Play of int list
  | Discard of int list
  | Buy of int
  | Sell of int
  | Use of int * int list
  | Reroll
  | Leave
[@@deriving show, eq]

type outcome =
  | Played of Play.t * int
  | BlindWon of Play.t * int * Payout.t
  | RunWon of Play.t * int
  | RunLost of Play.t * int
  | Done

let ( let* ) = Result.bind

(* Wraps the run of an action that has nothing to show. *)
let as_done result = Result.map (fun run -> Done, run) result

(* What a play caused, looking at the run after it. *)
let after_play rng (play, points, run) =
  match Run.status run, Blind.status (Run.blind run) with
  | Run.Won, _ -> Ok (RunWon (play, points), run)
  | Run.Lost, _ -> Ok (RunLost (play, points), run)
  | Run.InProgress, Blind.Won ->
    let* payout, shopping = Run.cash_out rng run in
    Ok (BlindWon (play, points, payout), shopping)
  | Run.InProgress, _ -> Ok (Played (play, points), run)
;;

let apply rng action run =
  match action with
  | Play indices ->
    let* played = Run.play indices run in
    after_play rng played
  | Discard indices -> Run.discard indices run |> as_done
  | Buy index -> Run.buy index run |> as_done
  | Sell index -> Run.sell_joker index run |> as_done
  | Use (index, indices) -> Run.use_consumable index indices run |> as_done
  | Reroll -> Run.reroll rng run |> as_done
  | Leave -> Run.leave_shop rng run |> as_done
;;
