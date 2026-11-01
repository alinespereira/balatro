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
  boss: Boss.t;
  past_bosses: Boss.t list;
  blind: Blind.t;
  levels: Poker.Levels.t;
  money: int;
  phase: phase;
  target_of: ante:int -> stage -> int;
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
  | NotUsable of Consumables.Consumable.t
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

let create ?(target = target) rng color =
  let run_ante = 1 in
  let run_stage = Small in
  let run_target = target ~ante:run_ante run_stage in
  let run_board = Board.create rng color in
  let run_blind = Blind.create ~target:run_target run_board in
  {
    ante = run_ante;
    stage = run_stage;
    boss = Boss.random rng;
    past_bosses = [];
    blind = run_blind;
    levels = Poker.Levels.initial;
    money = starting_money;
    phase = Playing;
    target_of = target;
  }
;;

let ante { ante; _ } = ante
let stage { stage; _ } = stage
let boss { boss; _ } = boss
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
  let new_levels = Poker.Levels.record_play (Play.hand the_play) the_levels in
  Ok (the_play, won_chips, { run with blind = the_blind; levels = new_levels })
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

(* What the shop can offer with this board: the jokers it already has are left out. *)
let shop_items board run =
  Shop.available ~jokers:(Board.jokers board) ~levels:(levels run)
;;

let cash_out rng run =
  let* () = not_shopping run in
  let { ante = the_ante; stage = the_stage; blind = the_blind; money = the_money; _ } =
    run
  in
  match Blind.status the_blind, advance the_ante the_stage with
  | Blind.InProgress, _ -> Error BlindNotOver
  | Blind.Lost, _
  | Blind.Won, None ->
    Error RunOver
  | Blind.Won, Some _ ->
    let the_reward = reward the_stage in
    let { Blind.plays_left; Blind.board = the_board; _ } = the_blind in
    let payout = Payout.make ~reward:the_reward ~plays_left ~money:the_money in
    let shop = Shop.create ~from:(shop_items the_board run) rng in
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
  | {
   phase = Shopping { board = the_board; _ };
   ante = the_ante;
   stage = the_stage;
   boss = the_boss;
   past_bosses = the_past_bosses;
   target_of;
   _;
  } ->
    (match advance the_ante the_stage with
     | None -> Error RunOver
     | Some (new_ante, new_stage) ->
       let new_boss, new_past_bosses =
         if new_ante = the_ante then
           the_boss, the_past_bosses
         else (
           let past = the_boss :: the_past_bosses in
           Boss.random ~avoid:past rng, past
         )
       in
       let new_target = target_of ~ante:new_ante new_stage in
       let new_board = Board.reset rng the_board in
       let blind_boss =
         match new_stage with
         | Boss -> Some new_boss
         | Small
         | Big ->
           None
       in
       let new_blind = Blind.create ?boss:blind_boss ~target:new_target new_board in
       Ok
         {
           run with
           ante = new_ante;
           stage = new_stage;
           boss = new_boss;
           past_bosses = new_past_bosses;
           blind = new_blind;
           phase = Playing;
         })
;;

let use_planet planet run =
  { run with levels = run |> levels |> Poker.Levels.use_planet planet }
;;

let buy idx run =
  let { phase = the_phase; money = the_money; _ } = run in
  match the_phase with
  | Playing -> Error NotInShop
  | Shopping { shop; board } ->
    let* item, new_shop = Shop.take idx shop |> Result.map_error (fun e -> ShopError e) in
    let item_price = Shop.price item in
    if item_price > the_money then
      Error (NotEnoughMoney item_price)
    else
      let* new_run, new_board =
        match item with
        | Shop.JokerItem joker ->
          Board.add_joker joker board
          |> Result.map (fun new_board -> run, new_board)
          |> Result.map_error (fun e -> BoardError e)
        | Shop.PlanetItem planet ->
          let new_run = use_planet planet run in
          Ok (new_run, board)
        | Shop.TarotItem tarot ->
          Board.add_consumable (Consumables.Consumable.Tarot tarot) board
          |> Result.map (fun new_board -> run, new_board)
          |> Result.map_error (fun e -> BoardError e)
      in
      Ok
        {
          new_run with
          money = the_money - item_price;
          phase = Shopping { shop = new_shop; board = new_board };
        }
;;

let sell_joker idx run =
  let { phase = the_phase; money = the_money; _ } = run in
  match the_phase with
  | Playing -> Error NotInShop
  | Shopping { shop; board } ->
    Board.remove_joker idx board
    |> Result.map (fun (joker, new_board) ->
      {
        run with
        money = the_money + Joker.sell_value joker;
        phase = Shopping { shop; board = new_board };
      })
    |> Result.map_error (fun e -> BoardError e)
;;

let reroll rng run =
  let { phase = the_phase; money = the_money; _ } = run in
  match the_phase with
  | Playing -> Error NotInShop
  | Shopping { shop; board } ->
    let { Shop.reroll_cost; _ } = shop in
    if reroll_cost > the_money then
      Error (NotEnoughMoney reroll_cost)
    else
      Ok
        {
          run with
          money = the_money - reroll_cost;
          phase =
            Shopping { shop = Shop.reroll ~from:(shop_items board run) rng shop; board };
        }
;;

(* The run with another board, in the blind being played or in the shop. *)
let with_board new_board run =
  match run.phase with
  | Playing -> { run with blind = Blind.with_board new_board run.blind }
  | Shopping { shop; _ } -> { run with phase = Shopping { shop; board = new_board } }
;;

let use_consumable idx indices run =
  let board_error e = BoardError e in
  let* consumable, board =
    Board.remove_consumable idx (board run) |> Result.map_error board_error
  in
  match consumable with
  | Consumables.Consumable.Tarot tarot ->
    let* new_board = Board.use_tarot tarot indices board |> Result.map_error board_error in
    let new_money = Consumables.Tarot.change_money tarot (money run) in
    Ok (with_board new_board { run with money = new_money })
  | Planet planet ->
    (match indices with
     | [] -> Ok (with_board board (use_planet planet run))
     | _ -> Error (BoardError (Board.WrongSelection { min = 0; max = 0; got = List.length indices })))
  | Spectral _ -> Error (NotUsable consumable)
;;
