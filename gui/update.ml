open Balatro
open Balatro_text

type event = Click of float * float

let toggle idx selected =
  match List.partition (( = ) idx) selected with
  | [], rest when List.length rest < Board.max_selected -> idx :: rest
  | _, rest -> rest
;;

(* The hand of a play and its points, such as ["Par: 42 pontos"]. *)
let scored play points =
  Printf.sprintf "%s: %d pontos" (Label.hand_name (Play.hand play)) points
;;

let message = function
  | Action.Played (play, points) -> Some (scored play points)
  | BlindWon (play, points, payout) ->
    Some
      (Printf.sprintf
         "Blind vencido! %s, +$%d"
         (scored play points)
         (Payout.total payout))
  | RunWon (play, points) -> Some ("Você venceu! " ^ scored play points)
  | RunLost (play, points) -> Some ("Você perdeu! " ^ scored play points)
  | Done -> None
;;

(* Runs the action on the run. It worked: the selection empties and the message says
   what happened. It was refused: only the message changes. *)
let act rng action ({ Model.run; _ } as model) : Model.t =
  match Action.apply rng action run with
  | Ok (outcome, run) -> { run; selected = []; message = message outcome }
  | Error e -> { model with message = Some (Label.error e) }
;;

(* A click while playing: on a card, on a button, or on nothing. *)
let click rng point ({ Model.run; selected; _ } as model) : Model.t =
  let hand_size = List.length (Board.hand (Run.board run)) in
  match Layout.card_at ~selected hand_size point with
  | Some idx -> { model with selected = toggle idx selected }
  | None ->
    let cards = List.sort compare selected in
    if Layout.contains Layout.play_button point then
      act rng (Action.Play cards) model
    else if Layout.contains Layout.discard_button point then
      act rng (Action.Discard cards) model
    else
      model
;;

let step rng (Click (x, y)) ({ Model.run; _ } as model) : Model.t =
  match Run.status run, Run.phase run with
  | Run.InProgress, Run.Playing -> click rng (x, y) model
  | Run.InProgress, Run.Shopping _ -> act rng Action.Leave model
  | (Run.Won | Run.Lost), _ -> model
;;
