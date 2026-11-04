open Balatro
open Balatro_text

type event = Click of float * float

let toggle idx selected =
  match List.partition (( = ) idx) selected with
  | [], rest when List.length rest < Board.max_selected -> idx :: rest
  | _, rest -> rest
;;

let message = function
  | Action.Played (play, chips) ->
    let hand_name = Label.hand_name (Play.hand play) in
    Some (Format.sprintf "%s" hand_name)
  | BlindWon (play, chips, payout) -> Some (Format.sprintf "Você venceu")
  | RunWon (play, chips) -> Some (Format.sprintf "Você venceu")
  | RunLost (play, chips) -> Some (Format.sprintf "Você perdeu")
  | Done -> None
;;

let step rng e { Model.run; selected; message } : Model.t =
  match Run.status run, Run.phase run with
  | Run.InProgress, Run.Playing ->
    (match e with
     | Click (x, y) ->
       let hand_size = Run.board run |> Board.hand_size in
       (match Layout.card_at ~selected hand_size (x, y) with
        | Some idx ->
          let selected = toggle idx selected in
          { run; selected; message }
        | None -> { run; selected; message }))
  | _ -> failwith "TODO: Update.step"
;;
