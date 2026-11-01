(** What the player can type at the prompt. *)

type t =
  | Play of int list (** [p 1 3 5]: plays the cards at those positions. *)
  | Discard of int list (** [d 2 4]: discards the cards at those positions. *)
  | Buy of int (** [c 1]: buys the shop offer at that position. *)
  | Sell of int (** [v 2]: sells the joker at that position. *)
  | Reroll (** [r]: replaces the shop offers. *)
  | Leave (** [s]: leaves the shop for the next blind. *)
  | Help (** [h] or [?]: lists the commands. *)
  | Quit (** [q]: leaves the game. *)

type error =
  | Empty (** The line had nothing but spaces. *)
  | UnknownCommand of string (** The first word is not a command. *)
  | NotAnIndex of string (** A word after the command is not a number. *)
  | ArgsProvided of string * string list (** Args were provided but not expected. *)
  | ExpectedOneIndex of string (** The command takes exactly one position. *)

(** Parses one line typed by the player. Positions are typed 1-based, as the hand is
    shown on screen, and come back 0-based, as [Run.play] and [Run.discard] expect. *)
val parse : string -> (t, error) result

(** A message to show when the line could not be parsed. *)
val error_message : error -> string

(** The text shown for [Help]. *)
val help : string
