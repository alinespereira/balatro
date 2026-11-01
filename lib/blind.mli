(** Uma rodada: bater [target] pontos antes de acabarem as jogadas. *)

type t = private {
  board: Board.t;
  target: int;
  plays_left: int;
  discards_left: int;
  chips: int;
  boss: Boss.t option; (** The boss of a [Boss] blind; [None] for the others. *)
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

(** Padrão: 4 jogadas, 3 descartes. With a [boss], the plays, discards and target go
    through [Boss.limits] first. *)
val create : ?boss:Boss.t -> ?plays:int -> ?discards:int -> target:int -> Board.t -> t

(** Joga as cartas dos índices; devolve a jogada, os pontos dela e a rodada nova. Os
    jokers do board entram na pontuação, da esquerda para a direita. With a boss, the cards it debuffs do
    not score and are not seen by the jokers, and [Boss.disables_jokers] leaves the
    jokers out. *)
val play : Poker.Levels.t -> int list -> t -> (Play.t * int * t, error) result

val discard : int list -> t -> (t, error) result

(** The same blind with another board, such as one where a tarot changed the hand. *)
val with_board : Board.t -> t -> t
val status : t -> status
val board : t -> Board.t
val target : t -> int
val chips : t -> int
val plays_left : t -> int
val discards_left : t -> int
val boss : t -> Boss.t option
