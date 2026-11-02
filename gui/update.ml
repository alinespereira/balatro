open Balatro

type event = Click of float * float

(* TODO(Aline): if [idx] is already in [selected], take it out; otherwise add it,
   unless there are already [Board.max_selected] cards. *)
let toggle (_ : int) (_ : int list) : int list = failwith "TODO: Update.toggle"

(* TODO(Aline): one string per outcome, in Portuguese. [Balatro_text.Label.hand_name]
   and [Payout.total] help. *)
let message (_ : Action.outcome) : string option = failwith "TODO: Update.message"

(* TODO(Aline): look at [Run.status] and [Run.phase] first. While playing, find what
   the click hit with [Layout.card_at] and [Layout.contains], and use [Action.apply]
   for the buttons. *)
let step (_ : Random.State.t) (_ : event) (_ : Model.t) : Model.t =
  failwith "TODO: Update.step"
;;
