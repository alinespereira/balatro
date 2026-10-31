type t =
  | Play of int list
  | Discard of int list
  | Help
  | Quit

type error =
  | Empty
  | UnknownCommand of string
  | NotAnIndex of string

let ( let* ) = Result.bind

let rec sequence = function
  | [] -> Ok []
  | r :: rs ->
    let* x = r in
    let* xs = sequence rs in
    Ok (x :: xs)
;;

let to_index arg =
  match int_of_string_opt arg with
  | Some n -> Ok (n - 1)
  | None -> Error (NotAnIndex arg)
;;

let parse line =
  let parsed =
    line |> String.trim |> String.split_on_char ' ' |> List.filter (fun w -> w <> "")
  in
  let indices args = List.map to_index args |> sequence in
  match parsed with
  | [] -> Error Empty
  | "p" :: args -> Result.map (fun idx -> Play idx) (indices args)
  | "d" :: args -> Result.map (fun idx -> Discard idx) (indices args)
  | "h" :: _ -> Ok Help
  | "q" :: _ -> Ok Quit
  | cmd :: _ -> Error (UnknownCommand cmd)
;;

let error_message = function
  | Empty -> "Por favor, informe um comando"
  | UnknownCommand cmd -> Printf.sprintf "Comando inválido: '%s'" cmd
  | NotAnIndex idx -> Printf.sprintf "Não é uma posição válida: '%s'" idx
;;

let help =
  {|Commands:
  p 1 3 5   play the cards at positions 1, 3 and 5
  d 2 4     discard the cards at positions 2 and 4
  h         show this help
  q         quit|}
;;
