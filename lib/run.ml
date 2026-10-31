type stage =
  | Small
  | Big
  | Boss
[@@deriving show, eq]

type phase =
  | Playing
  | Shopping of {
      shop: Shop.t;
      board: Board.t;
    }

type t = {
  ante: int;
  stage: stage;
  blind: Blind.t;
  levels: Poker.Levels.t;
  money: int;
  phase: phase;
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
  | InShop
  | NotInShop
  | NotEnoughMoney of int
  | ShopError of Shop.error
  | BoardError of Board.error
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
    phase = Playing;
  }
;;

let ante { ante; _ } = ante
let stage { stage; _ } = stage
let blind { blind; _ } = blind
let levels { levels; _ } = levels
let money { money; _ } = money
let phase { phase; _ } = phase

let board = function
  | { phase = Shopping { board; _ }; _ } -> board
  | { phase = Playing; blind; _ } -> Blind.board blind
;;

let status run =
  let { ante; stage; blind; _ } = run in
  match Blind.status blind, stage with
  | Blind.Lost, _ -> Lost
  | Blind.Won, Boss when ante = final_ante -> Won
  | _ -> InProgress
;;

let not_shopping = function
  | { phase = Playing; _ } -> Ok ()
  | { phase = Shopping _; _ } -> Error InShop
;;

let play indices run =
  let* () = not_shopping run in
  let { blind = the_blind; levels = the_levels; _ } = run in
  let* the_play, won_chips, the_blind =
    the_blind |> Blind.play the_levels indices |> Result.map_error (fun e -> BlindError e)
  in
  Ok (the_play, won_chips, { run with blind = the_blind })
;;

let discard indices run =
  let* () = not_shopping run in
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

let cash_out rng run =
  let* () = not_shopping run in
  let { ante = the_ante; stage = the_stage; blind = the_blind; money = the_money; _ } =
    run
  in
  match Blind.status the_blind, advance the_ante the_stage with
  | InProgress, _ -> Error BlindNotOver
  | Lost, _
  | Won, None ->
    Error RunOver
  | Won, Some _ ->
    let the_reward = reward the_stage in
    let { Blind.plays_left; Blind.board = the_board; _ } = the_blind in
    let payout = Payout.make ~reward:the_reward ~plays_left ~money:the_money in
    let shop = Shop.create rng in
    Ok
      ( payout,
        {
          run with
          money = the_money + Payout.total payout;
          phase = Shopping { shop; board = the_board };
        } )
;;

let leave_shop rng run =
  match run with
  | { phase = Playing; _ } -> Error NotInShop
  | { phase = Shopping { board = the_board; _ }; ante = the_ante; stage = the_stage; _ }
    ->
    (match advance the_ante the_stage with
     | None -> Error RunOver
     | Some (new_ante, new_stage) ->
       let new_target = target new_stage ~ante:new_ante in
       let new_board = Board.reset rng the_board in
       let new_blind = Blind.create ~target:new_target new_board in
       Ok
         {
           run with
           ante = new_ante;
           stage = new_stage;
           blind = new_blind;
           phase = Playing;
         })
;;

let use_planet planet run =
  { run with levels = run |> levels |> Poker.Levels.use_planet planet }
;;

(* TODO: [NotInShop] while playing. Take the offer from the shop ([ShopError] if there
   is none), check the money against [Shop.price] ([NotEnoughMoney]), then pay and:
   - a joker goes to the shop's board with [Board.add_joker] ([BoardError] if full);
   - a planet is used right away with [use_planet].
   Nothing changes when any step fails: the offer stays and the money is not spent. *)
let buy _ _ = failwith "Run.buy: not implemented"

(* TODO: [NotInShop] while playing. Remove the joker from the shop's board with
   [Board.remove_joker] ([BoardError] if there is none) and add its
   [Joker.sell_value] to the money. *)
let sell_joker _ _ = failwith "Run.sell_joker: not implemented"

(* TODO: [NotInShop] while playing, [NotEnoughMoney] without [reroll_cost]. Pay the
   cost from before the reroll, then [Shop.reroll]. *)
let reroll _ _ = failwith "Run.reroll: not implemented"
