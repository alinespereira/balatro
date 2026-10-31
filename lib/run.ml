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
  money: int;
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

let ( let* ) = Result.bind
let final_ante = 8
let starting_money = 4

let reward = function
  | Small -> 3
  | Big -> 4
  | Boss -> 5
;;

let score_multiplier stage =
  match stage with
  | Small -> 1.0
  | Big -> 1.5
  | Boss -> 2.0
;;

let ante_base = function
  | 1 -> 300
  | 2 -> 800
  | 3 -> 2_000
  | 4 -> 5_000
  | 5 -> 11_000
  | 6 -> 20_000
  | 7 -> 35_000
  | 8 -> 50_000
  | ante -> invalid_arg (Printf.sprintf "Run.target: no ante %d" ante)
;;

let target ~ante stage =
  Float.of_int (ante_base ante) *. score_multiplier stage |> Int.of_float
;;

let create rng color =
  let run_ante = 1 in
  let run_stage = Small in
  let run_target = target run_stage ~ante:run_ante in
  let run_board = Board.create rng color in
  let run_blind = Blind.create ~target:run_target run_board in
  {
    ante = run_ante;
    stage = run_stage;
    blind = run_blind;
    levels = Poker.Levels.initial;
    money = starting_money;
  }
;;

let ante { ante; _ } = ante
let stage { stage; _ } = stage
let blind { blind; _ } = blind
let levels { levels; _ } = levels
let money { money; _ } = money

let status run =
  let { ante; stage; blind; _ } = run in
  match Blind.status blind, stage with
  | Blind.Lost, _ -> Lost
  | Blind.Won, Boss when ante = final_ante -> Won
  | _ -> InProgress
;;

let play indices run =
  let { blind = the_blind; levels = the_levels; _ } = run in
  let* the_play, won_chips, the_blind =
    the_blind |> Blind.play the_levels indices |> Result.map_error (fun e -> BlindError e)
  in
  Ok (the_play, won_chips, { run with blind = the_blind })
;;

let discard indices run =
  let { blind = the_blind; _ } = run in
  let* the_blind =
    the_blind |> Blind.discard indices |> Result.map_error (fun e -> BlindError e)
  in
  Ok { run with blind = the_blind }
;;

let advance ante = function
  | Small -> Some (ante, Big)
  | Big -> Some (ante, Boss)
  | Boss when ante < final_ante -> Some (ante + 1, Small)
  | Boss -> None
;;

let next_blind rng run =
  let { ante = the_ante; stage = the_stage; blind = the_blind; money = the_money; _ } =
    run
  in
  match Blind.status the_blind, advance the_ante the_stage with
  | InProgress, _ -> Error BlindNotOver
  | Lost, _
  | Won, None ->
    Error RunOver
  | Won, Some (new_ante, new_stage) ->
    let the_reward = reward the_stage in
    let { Blind.plays_left; Blind.board = the_board; _ } = the_blind in
    let money = the_money in
    let payout = Payout.make ~reward:the_reward ~plays_left ~money in
    let new_target = target new_stage ~ante:new_ante in
    let new_board = Board.reset rng the_board in
    let new_blind = Blind.create ~target:new_target new_board in
    Ok
      ( payout,
        {
          run with
          ante = new_ante;
          stage = new_stage;
          blind = new_blind;
          money = money + Payout.total payout;
        } )
;;

let use_planet planet run =
  { run with levels = run |> levels |> Poker.Levels.use_planet planet }
;;
