type t =
  | Play of int list
  | Discard of int list
  | Help
  | Quit

type error =
  | Empty
  | UnknownCommand of string
  | NotAnIndex of string

(* TODO: split the line into words, match on the first one and turn the rest into
   0-based indices. *)
let parse _line = failwith "TODO: Command.parse"

(* TODO: one message per constructor. *)
let error_message _error = failwith "TODO: Command.error_message"

let help =
  {|Commands:
  p 1 3 5   play the cards at positions 1, 3 and 5
  d 2 4     discard the cards at positions 2 and 4
  h         show this help
  q         quit|}
;;
