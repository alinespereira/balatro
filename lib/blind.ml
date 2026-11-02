type t = {
  board: Board.t;
  target: int;
  plays_left: int;
  discards_left: int;
  chips: int;
  boss: Boss.t option;
}

type status =
  | InProgress
  | Won
  | Lost
[@@deriving show, eq]

type error =
  | NoPlaysLeft
  | NoDiscardsLeft
  | RoundOver
  | BoardError of Board.error
[@@deriving show]

let ( let* ) = Result.bind
let default_plays = 4
let default_discards = 3

let create ?boss ?(plays = default_plays) ?(discards = default_discards) ~target board =
  let limits = { Boss.plays; Boss.discards; Boss.target } in
  let { Boss.plays; Boss.discards; Boss.target } =
    match boss with
    | None -> limits
    | Some boss -> Boss.limits boss limits
  in
  {
    board = Board.refill board;
    target;
    plays_left = plays;
    discards_left = discards;
    chips = 0;
    boss;
  }
;;

let board { board; _ } = board
let with_board board blind = { blind with board }
let target { target; _ } = target
let chips { chips; _ } = chips
let plays_left { plays_left; _ } = plays_left
let discards_left { discards_left; _ } = discards_left
let boss { boss; _ } = boss

let debuff_play boss play =
  match boss with
  | None -> play
  | Some boss -> Play.without (Boss.debuffs boss) play
;;

let jokers_disabled = function
  | None -> false
  | Some boss -> Boss.disables_jokers boss
;;

let active_jokers boss jokers =
  if jokers_disabled boss then
    []
  else
    jokers
;;

(* Jokers silenced by a boss do not score, and do not react either. *)
let react_jokers boss event board =
  if jokers_disabled boss then
    board
  else
    Board.update_jokers (Joker.react event) board
;;

let status = function
  | { chips; target; _ } when chips >= target -> Won
  | { plays_left; _ } when plays_left <= 0 -> Lost
  | _ -> InProgress
;;

let play levels indices blind =
  match status blind with
  | Lost -> Error NoPlaysLeft
  | Won -> Error RoundOver
  | InProgress ->
    let* play, board =
      blind |> board |> Board.play indices |> Result.map_error (fun e -> BoardError e)
    in
    (* The play keeps every card, so it can be shown; only the scoring sees the
       debuffs. *)
    let scored = debuff_play blind.boss play in
    let context =
      {
        Joker.play = scored;
        Joker.discards_left = blind.discards_left;
        Joker.jokers = active_jokers blind.boss (Board.jokers board);
      }
    in
    let score = Play.score levels scored |> Score.apply_all (Joker.all_changes context) in
    let play_chips = Score.total score in
    (* The jokers react after the play has scored: a Green Joker bought with +0 mult
       scores 0 on its first hand and 1 on the second. *)
    let board = react_jokers blind.boss (Joker.Played scored) board in
    Ok
      ( play,
        play_chips,
        {
          blind with
          board = Board.refill board;
          plays_left = blind.plays_left - 1;
          chips = blind.chips + play_chips;
        } )
;;

let discard indices blind =
  match status blind, discards_left blind with
  | Lost, _
  | Won, _ ->
    Error RoundOver
  | InProgress, discards_left when discards_left <= 0 -> Error NoDiscardsLeft
  | InProgress, discards_left ->
    let* board =
      blind |> board |> Board.discard indices |> Result.map_error (fun e -> BoardError e)
    in
    let board = react_jokers blind.boss Joker.Discarded board in
    Ok { blind with board = Board.refill board; discards_left = discards_left - 1 }
;;
