(** What the player can type at the prompt. *)

type t =
  | Play of int list (** [p 1 3 5]: plays the cards at those positions. *)
  | Discard of int list (** [d 2 4]: discards the cards at those positions. *)
  | Help (** [h] or [?]: lists the commands. *)
  | Quit (** [q]: leaves the game. *)

type error =
  | Empty (** The line had nothing but spaces. *)
  | UnknownCommand of string (** The first word is not a command. *)
  | NotAnIndex of string (** A word after the command is not a number. *)

(** Parses one line typed by the player. Positions are typed 1-based, as the hand is
    shown on screen, and come back 0-based, as [Run.play] and [Run.discard] expect. *)
val parse : string -> (t, error) result

(** A message to show when the line could not be parsed. *)
val error_message : error -> string

(** The text shown for [Help]. *)
val help : string
