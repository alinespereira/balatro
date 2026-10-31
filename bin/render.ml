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

let joker_name = function
  | Joker.Joker -> "Coringa"
  | GreedyJoker -> "Coringa Ganancioso"
  | LustyJoker -> "Coringa Luxurioso"
  | WrathfulJoker -> "Coringa Irado"
  | GluttonousJoker -> "Coringa Guloso"
  | JollyJoker -> "Coringa Alegre"
  | HalfJoker -> "Meio Coringa"
  | Banner -> "Estandarte"
  | AbstractJoker -> "Coringa Abstrato"
  | Cavendish -> "Cavendish"
;;

let joker_description = function
  | Joker.Joker -> "+4 mult"
  | GreedyJoker -> "+3 mult por carta de ♦ que pontua"
  | LustyJoker -> "+3 mult por carta de ♥ que pontua"
  | WrathfulJoker -> "+3 mult por carta de ♠ que pontua"
  | GluttonousJoker -> "+3 mult por carta de ♣ que pontua"
  | JollyJoker -> "+8 mult se a mão for um par"
  | HalfJoker -> "+20 mult se jogar 3 cartas ou menos"
  | Banner -> "+30 fichas por descarte restante"
  | AbstractJoker -> "+3 mult por joker que você tem"
  | Cavendish -> "x3 mult"
;;

let jokers = function
  | [] -> ""
  | jokers ->
    jokers
    |> List.mapi (fun i joker ->
      Printf.sprintf "  %d. %s: %s" (i + 1) (joker_name joker) (joker_description joker))
    |> String.concat "\n"
;;

let planet_name = function
  | Consumables.Planet.Pluto -> "Plutão"
  | Mercury -> "Mercúrio"
  | Uranus -> "Urano"
  | Venus -> "Vênus"
  | Saturn -> "Saturno"
  | Jupiter -> "Júpiter"
  | Earth -> "Terra"
  | Mars -> "Marte"
  | Neptune -> "Netuno"
  | PlanetX -> "Planeta X"
  | Ceres -> "Ceres"
  | Eris -> "Éris"
;;

(* The hands a planet levels up: Neptune is the only one with two. *)
let planet_hands planet =
  Poker.Hand.all
  |> List.filter (fun hand -> Poker.Hand.planet hand = planet)
  |> List.map Poker.Hand.show
  |> String.concat " e "
;;

let item = function
  | Shop.JokerItem joker ->
    Printf.sprintf "%s: %s" (joker_name joker) (joker_description joker)
  | PlanetItem planet ->
    Printf.sprintf "%s: sobe o nível de %s" (planet_name planet) (planet_hands planet)
;;

let shop (the_shop : Shop.t) =
  let offers =
    the_shop.offers
    |> List.mapi (fun i offer ->
      Printf.sprintf "  %d. $%d  %s" (i + 1) (Shop.price offer) (item offer))
  in
  String.concat
    "\n"
    (("Loja:" :: offers) @ [Printf.sprintf "  Rerrolar: $%d" the_shop.reroll_cost])
;;

let status run =
  let { Run.ante; Run.stage; Run.blind; Run.money; _ } = run in
  let { Blind.target; Blind.plays_left; Blind.discards_left; Blind.chips; _ } = blind in
  Printf.sprintf
    {|
    Ante: %d
    Stage: %s
    Target: %d

    Chips: %d
    Plays: %d
    Discards: %d
    Money: $%d
  |}
    ante
    (Run.show_stage stage)
    target
    chips
    plays_left
    discards_left
    money
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

let payout payout =
  let { Payout.blind; Payout.plays; Payout.interest } = payout in
  Printf.sprintf
    {|
    Recompensa do blind: $%d
    Jogadas restantes: $%d
    Juros: $%d
    Total: $%d
  |}
    blind
    plays
    interest
    (Payout.total payout)
;;

let board_error : Board.error -> string = function
  | Board.NoCards -> "Escolha pelo menos uma carta"
  | TooManyCards n ->
    Printf.sprintf "No máximo %d cartas, mas você escolheu %d" Board.max_selected n
  | InvalidIndex i -> Printf.sprintf "Não há carta na posição %d" (i + 1)
  | DuplicateIndex i -> Printf.sprintf "A posição %d foi escolhida mais de uma vez" (i + 1)
  | InvalidPlay (Poker.Hand.InvalidCardCount n) ->
    Printf.sprintf "Uma jogada precisa de 1 a 5 cartas, mas tem %d" n
  | NoJokerSlot -> "Não há espaço para mais jokers"
  | NoJokerAt i -> Printf.sprintf "Não há joker na posição %d" (i + 1)
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
  | InShop -> "Você está na loja: compre, venda, rerrole ou saia com 's'"
  | NotInShop -> "Esse comando só funciona na loja"
  | NotEnoughMoney price -> Printf.sprintf "Dinheiro insuficiente: custa $%d" price
  | ShopError (Shop.NoOfferAt i) -> Printf.sprintf "Não há oferta na posição %d" (i + 1)
  | BoardError e -> board_error e
;;
