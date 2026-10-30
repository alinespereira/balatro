(** A whole run: a sequence of blinds that get harder as the run goes on.

    Every ante has three blinds, played in order: [Small], [Big] and [Boss]. Beating
    the boss blind of [final_ante] wins the run; losing any blind loses it. Hand levels
    carry over from one blind to the next, but every blind starts with a new board and
    a freshly shuffled deck. *)

(** Which of the three blinds of an ante is being played. *)
type stage =
  | Small
  | Big
  | Boss
[@@deriving show, eq]

(** The current ante and stage, the blind being played and the hand levels. *)
type t = {
  ante: int;
  stage: stage;
  blind: Blind.t;
  levels: Poker.Levels.t;
}

(** [InProgress] also covers a blind that was just beaten and is waiting for
    [next_blind]; check [Blind.status (blind run)] to tell the two apart. *)
type status =
  | InProgress
  | Won
  | Lost
[@@deriving show, eq]

type error =
  | BlindError of Blind.error (** The current blind refused the play or the discard. *)
  | BlindNotOver (** [next_blind] was called before the current blind was beaten. *)
  | RunOver (** The run is already won or lost. *)
[@@deriving show]

(** The last ante: beating its boss blind wins the run. *)
val final_ante : int

(** Points needed to beat the blind at [stage] of [ante]. Grows with the ante, and
    within an ante [Small < Big < Boss]. *)
val target : ante:int -> stage -> int

(** A run at the small blind of ante 1, every hand at level 1. *)
val create : Random.State.t -> Deck.color -> t

val ante : t -> int
val stage : t -> stage
val blind : t -> Blind.t
val levels : t -> Poker.Levels.t
val status : t -> status

(** Plays the cards at the given positions of the hand in the current blind, scored
    with the run's hand levels. Returns the play, its points and the new run. *)
val play : int list -> t -> (Play.t * int * t, error) result

(** Discards the cards at the given positions of the hand in the current blind. *)
val discard : int list -> t -> (t, error) result

(** Moves on to the next blind once the current one is beaten: [Small] to [Big],
    [Big] to [Boss], and [Boss] to the [Small] blind of the next ante. The new blind
    gets a new board with a deck of the same color, shuffled with the given state. *)
val next_blind : Random.State.t -> t -> (t, error) result

(** Levels up the hands of the planet. The new levels count from the next play on. *)
val use_planet : Consumables.Planet.t -> t -> t
