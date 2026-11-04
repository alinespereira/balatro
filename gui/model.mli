(** Everything the window shows: the run, plus what only the screen cares about. *)

type t = {
  run: Balatro.Run.t;
  selected: int list;
    (** The positions (0-based) of the selected cards of the hand, in the order they
          were clicked. *)
  message: string option; (** What the last click caused, such as ["Par: 42 pontos"]. *)
}

(** A model for the run, with no card selected and no message. *)
val create : Balatro.Run.t -> t
