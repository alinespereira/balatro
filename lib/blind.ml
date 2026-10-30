type t = {
  board: Board.t;
  target: int;
  plays_left: int;
  discards_left: int;
  chips: int;
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

let create ?(plays = default_plays) ?(discards = default_discards) ~target board =
  {
    board = Board.refill board;
    target;
    plays_left = plays;
    discards_left = discards;
    chips = 0;
  }
;;

let board { board; _ } = board
let target { target; _ } = target
let chips { chips; _ } = chips
let plays_left { plays_left; _ } = plays_left
let discards_left { discards_left; _ } = discards_left

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
    let score = Play.score levels play in
    let play_chips = Score.total score in
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
    Ok { blind with board = Board.refill board; discards_left = discards_left - 1 }
;;
