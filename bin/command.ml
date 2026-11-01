type t =
  | Play of int list
  | Discard of int list
  | Buy of int
  | Sell of int
  | Reroll
  | Leave
  | Help
  | Quit

type error =
  | Empty
  | UnknownCommand of string
  | NotAnIndex of string
  | ArgsProvided of string * string list
  | ExpectedOneIndex of string

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
  | ["c"; arg] -> Result.map (fun idx -> Buy idx) (to_index arg)
  | ["v"; arg] -> Result.map (fun idx -> Sell idx) (to_index arg)
  | (("c" | "v") as cmd) :: _ -> Error (ExpectedOneIndex cmd)
  | ["r"] -> Ok Reroll
  | ["s"] -> Ok Leave
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
;;

let help =
  {|Comandos:
  p 1 3 5   joga as cartas nas posições 1, 3 e 5
  d 2 4     descarta as cartas nas posições 2 e 4
  h         mostra esta ajuda
  ?         mostra esta ajuda
  q         sai do jogo

Na loja:
  c 1       compra a oferta na posição 1
  v 2       vende o joker na posição 2
  r         troca as ofertas (rerrolar)
  s         sai da loja e vai para o próximo blind|}
;;
