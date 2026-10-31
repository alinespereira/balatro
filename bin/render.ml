open Balatro

let suit = function
  | Card.Clubs -> "♣"
  | Card.Diamonds -> "♦"
  | Card.Hearts -> "♥"
  | Card.Spades -> "♠"
;;

let rank = function
  | Card.Two -> "2"
  | Card.Three -> "3"
  | Card.Four -> "4"
  | Card.Five -> "5"
  | Card.Six -> "6"
  | Card.Seven -> "7"
  | Card.Eight -> "8"
  | Card.Nine -> "9"
  | Card.Ten -> "10"
  | Card.Jack -> "J"
  | Card.Queen -> "Q"
  | Card.King -> "K"
  | Card.Ace -> "A"
;;

let card { Card.suit = s; Card.rank = r } = rank r ^ suit s

let hand cards =
  let sep = "|" in
  let cards_row =
    List.map
      (fun { Card.suit = s; Card.rank = r } ->
         Printf.sprintf "  %2s%s " (rank r) (suit s))
      cards
    |> String.concat sep
  in
  let indices_row =
    List.mapi (fun i _ -> Printf.sprintf "  %3d " (i + 1)) cards |> String.concat sep
  in
  let divider_row = String.make (String.length indices_row) '-' in
  String.concat "\n" [indices_row; divider_row; cards_row]
;;

let status run =
  let { Run.ante; Run.stage; Run.blind; _ } = run in
  let { Blind.target; Blind.plays_left; Blind.discards_left; Blind.chips; _ } = blind in
  Printf.sprintf
    {|
    Ante: %d
    Stage: %s
    Target: %d

    Chips: %d
    Plays: %d
    Discards: %d
  |}
    ante
    (Run.show_stage stage)
    target
    chips
    plays_left
    discards_left
;;

let play play points =
  let hand = Play.hand play |> Poker.Hand.show in
  Printf.sprintf
    {|
    Hand: %s
    Chips: %d
  |}
    hand
    points
;;

let board_error : Board.error -> string = function
  | Board.NoCards -> "Escolha pelo menos uma carta"
  | TooManyCards n ->
    Printf.sprintf "No máximo %d cartas, mas você escolheu %d" Board.max_selected n
  | InvalidIndex i -> Printf.sprintf "Não há carta na posição %d" (i + 1)
  | DuplicateIndex i -> Printf.sprintf "A posição %d foi escolhida mais de uma vez" (i + 1)
  | InvalidPlay (Poker.Hand.InvalidCardCount n) ->
    Printf.sprintf "Uma jogada precisa de 1 a 5 cartas, mas tem %d" n
;;

let blind_error : Blind.error -> string = function
  | Blind.NoPlaysLeft -> "Não há mais jogadas nesta rodada"
  | NoDiscardsLeft -> "Não há mais descartes nesta rodada"
  | RoundOver -> "A rodada já terminou"
  | BoardError e -> board_error e
;;

let error : Run.error -> string = function
  | Run.BlindError e -> blind_error e
  | BlindNotOver -> "A rodada ainda não terminou"
  | RunOver -> "A partida já terminou"
;;
