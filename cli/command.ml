module Action = Balatro.Action

type t =
  | Act of Action.t
  | Help
  | Quit
[@@deriving show, eq]

type error =
  | Empty
  | UnknownCommand of string
  | NotAnIndex of string
  | ArgsProvided of string * string list
  | ExpectedOneIndex of string
  | MissingConsumable
[@@deriving show, eq]

let ( let* ) = Result.bind

let rec sequence = function
  | [] -> Ok []
  | r :: rs ->
    let* x = r in
    let* xs = sequence rs in
    Ok (x :: xs)
;;

(* Positions on screen start at 1, so 0 and negative numbers are rejected here instead
   of becoming an index that [Board] would report as a missing card. *)
let to_index arg =
  match int_of_string_opt arg with
  | Some n when n >= 1 -> Ok (n - 1)
  | Some _
  | None ->
    Error (NotAnIndex arg)
;;

let parse line =
  let parsed =
    line
    |> String.map (fun c ->
      if c = '\t' then
        ' '
      else
        c)
    |> String.split_on_char ' '
    |> List.filter (fun w -> w <> "")
  in
  let indices args = List.map to_index args |> sequence in
  match parsed with
  | [] -> Error Empty
  | "p" :: args -> Result.map (fun idx -> Act (Action.Play idx)) (indices args)
  | "d" :: args -> Result.map (fun idx -> Act (Action.Discard idx)) (indices args)
  | ["c"; arg] -> Result.map (fun idx -> Act (Action.Buy idx)) (to_index arg)
  | ["v"; arg] -> Result.map (fun idx -> Act (Action.Sell idx)) (to_index arg)
  | (("c" | "v") as cmd) :: _ -> Error (ExpectedOneIndex cmd)
  | ["u"] -> Error MissingConsumable
  | "u" :: consumable :: args ->
    let* idx = to_index consumable in
    let* cards = indices args in
    Ok (Act (Action.Use (idx, cards)))
  | ["r"] -> Ok (Act Action.Reroll)
  | ["s"] -> Ok (Act Action.Leave)
  | [("h" | "?")] -> Ok Help
  | ["q"] -> Ok Quit
  | (("r" | "s" | "h" | "?" | "q") as cmd) :: args -> Error (ArgsProvided (cmd, args))
  | cmd :: _ -> Error (UnknownCommand cmd)
;;

let error_message = function
  | Empty -> "Por favor, informe um comando"
  | UnknownCommand cmd -> Printf.sprintf "Comando inválido: '%s'" cmd
  | NotAnIndex idx -> Printf.sprintf "Não é uma posição válida: '%s'" idx
  | ArgsProvided (cmd, args) ->
    Printf.sprintf
      "Comando '%s' não aceita argumentos, mas recebeu '%s'"
      cmd
      (String.concat " " args)
  | ExpectedOneIndex cmd ->
    Printf.sprintf "O comando '%s' precisa de exatamente uma posição" cmd
  | MissingConsumable ->
    "Diga qual consumível usar: 'u 1', ou 'u 1 2 3' para as cartas 2 e 3"
;;

let help =
  {|Comandos:
  p 1 3 5   joga as cartas nas posições 1, 3 e 5
  d 2 4     descarta as cartas nas posições 2 e 4
  u 1 2 3   usa o consumível 1 nas cartas 2 e 3 (também na loja)
  h ou ?    mostra esta ajuda
  q         sai do jogo

Na loja:
  c 1       compra a oferta na posição 1
  v 2       vende o joker na posição 2
  r         troca as ofertas (rerrolar)
  s         sai da loja e vai para o próximo blind|}
;;
