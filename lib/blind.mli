(** Uma rodada: bater [target] pontos antes de acabarem as jogadas. *)

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

(** Padrão: 4 jogadas, 3 descartes. *)
val create : ?plays:int -> ?discards:int -> target:int -> Board.t -> t

(** Joga as cartas dos índices; devolve a jogada, os pontos dela e a rodada nova. *)
val play : Poker.Levels.t -> int list -> t -> (Play.t * int * t, error) result

val discard : int list -> t -> (t, error) result
val status : t -> status
val board : t -> Board.t
val target : t -> int
val chips : t -> int
val plays_left : t -> int
val discards_left : t -> int
