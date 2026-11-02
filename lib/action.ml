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

(* TODO(Aline): one case per action. The simple ones call the [Run] function and wrap
   the new run as [Done]. [Play] is the flow that lives in [bin/main.ml] today: after
   [Run.play], look at [Run.status] and at [Blind.status (Run.blind run)] to choose
   between [Played], [BlindWon] (after [Run.cash_out]), [RunWon] and [RunLost]. *)
let apply (_ : Random.State.t) (_ : t) (_ : Run.t) : (outcome * Run.t, Run.error) result =
  failwith "TODO: Action.apply"
;;
