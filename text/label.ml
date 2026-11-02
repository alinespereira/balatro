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
  | GreenJoker _ -> "Coringa Verde"
  | RideTheBus _ -> "Passeio de Ônibus"
  | Runner _ -> "Corredor"
  | IceCream _ -> "Sorvete"
;;

let joker_description = function
  | Joker.Joker -> "+4 mult"
  | GreedyJoker -> "+3 mult por carta de ♦ que pontua"
  | LustyJoker -> "+3 mult por carta de ♥ que pontua"
  | WrathfulJoker -> "+3 mult por carta de ♠ que pontua"
  | GluttonousJoker -> "+3 mult por carta de ♣ que pontua"
  | JollyJoker -> "+8 mult se a mão tiver um par"
  | HalfJoker -> "+20 mult se jogar 3 cartas ou menos"
  | Banner -> "+30 fichas por descarte restante"
  | AbstractJoker -> "+3 mult por joker que você tem"
  | Cavendish -> "x3 mult"
  | GreenJoker { mult } ->
    Printf.sprintf "+%d mult; +1 por mão jogada, -1 por descarte" mult
  | RideTheBus { mult } ->
    Printf.sprintf "+%d mult; +1 por mão seguida sem figura que pontua" mult
  | Runner { chips } -> Printf.sprintf "+%d fichas; +15 por mão com sequência" chips
  | IceCream { chips } ->
    Printf.sprintf "+%d fichas; -5 por mão jogada, derrete no 0" chips
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

let tarot_name = function
  | Consumables.Tarot.TheFool -> "O Louco"
  | TheMagician -> "O Mago"
  | TheHighPriestess -> "A Sacerdotisa"
  | TheEmpress -> "A Imperatriz"
  | TheEmperor -> "O Imperador"
  | TheHierophant -> "O Hierofante"
  | TheLovers -> "Os Enamorados"
  | TheChariot -> "O Carro"
  | Justice -> "A Justiça"
  | TheHermit -> "O Eremita"
  | TheWheelOfFortune -> "A Roda da Fortuna"
  | Strength -> "A Força"
  | TheHangedMan -> "O Enforcado"
  | Death -> "A Morte"
  | Temperance -> "A Temperança"
  | TheDevil -> "O Diabo"
  | TheTower -> "A Torre"
  | TheStar -> "A Estrela"
  | TheMoon -> "A Lua"
  | TheSun -> "O Sol"
  | Judgement -> "O Julgamento"
  | TheWorld -> "O Mundo"
;;

let tarot_description = function
  | Consumables.Tarot.TheStar -> "até 3 cartas viram ♦"
  | TheMoon -> "até 3 cartas viram ♣"
  | TheSun -> "até 3 cartas viram ♥"
  | TheWorld -> "até 3 cartas viram ♠"
  | Strength -> "até 2 cartas sobem um valor"
  | TheHangedMan -> "destrói até 2 cartas"
  | Death -> "de 2 cartas, a da esquerda vira uma cópia da da direita"
  | TheHermit -> "dobra o dinheiro (no máximo +$20)"
  | TheFool
  | TheMagician
  | TheHighPriestess
  | TheEmpress
  | TheEmperor
  | TheHierophant
  | TheLovers
  | TheChariot
  | Justice
  | TheWheelOfFortune
  | Temperance
  | TheDevil
  | TheTower
  | Judgement ->
    "ainda não faz nada"
;;

let boss_name = function
  | Boss.Drought -> "A Seca"
  | Hourglass -> "A Ampulheta"
  | Giant -> "O Gigante"
  | Eclipse _ -> "O Eclipse"
  | Fog -> "A Névoa"
  | Muzzle -> "A Mordaça"
;;

let boss_description = function
  | Boss.Drought -> "sem descartes"
  | Hourglass -> "uma jogada a menos"
  | Giant -> "meta 50% maior"
  | Eclipse s -> Printf.sprintf "cartas de %s não pontuam" (suit s)
  | Fog -> "figuras (J, Q, K) não pontuam"
  | Muzzle -> "os jokers não fazem nada"
;;

let stage = function
  | Run.Small -> "Small Blind"
  | Big -> "Big Blind"
  | Boss -> "Boss Blind"
;;

let hand_name = function
  | Poker.Hand.HighCard -> "Carta alta"
  | Pair -> "Par"
  | TwoPair -> "Dois pares"
  | ThreeOfAKind -> "Trinca"
  | Straight -> "Sequência"
  | Flush -> "Flush"
  | FullHouse -> "Full house"
  | FourOfAKind -> "Quadra"
  | StraightFlush -> "Straight flush"
  | RoyalFlush -> "Royal flush"
  | FiveOfAKind -> "Quina"
  | FlushHouse -> "Flush house"
  | FlushFive -> "Flush quina"
;;

(* The hands a planet levels up: Neptune is the only one with two. *)
let planet_hands planet =
  Poker.Hand.all
  |> List.filter (fun hand -> Poker.Hand.planet hand = planet)
  |> List.map hand_name
  |> String.concat " e "
;;

let consumable = function
  | Consumables.Consumable.Tarot tarot ->
    Printf.sprintf "%s: %s" (tarot_name tarot) (tarot_description tarot)
  | Planet planet ->
    Printf.sprintf "%s: sobe o nível de %s" (planet_name planet) (planet_hands planet)
  | Spectral spectral -> Consumables.Spectral.show spectral
;;

let item = function
  | Shop.JokerItem joker ->
    Printf.sprintf "%s: %s" (joker_name joker) (joker_description joker)
  | PlanetItem planet -> consumable (Consumables.Consumable.Planet planet)
  | TarotItem tarot -> consumable (Consumables.Consumable.Tarot tarot)
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
  | NoConsumableSlot -> "Não há espaço para mais consumíveis"
  | NoConsumableAt i -> Printf.sprintf "Não há consumível na posição %d" (i + 1)
  | WrongSelection { min; max; got } when min = max ->
    Printf.sprintf "Escolha %d cartas, mas você escolheu %d" min got
  | WrongSelection { min; max; got } ->
    Printf.sprintf "Escolha de %d a %d cartas, mas você escolheu %d" min max got
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
  | NotUsable c -> Printf.sprintf "%s ainda não pode ser usado" (consumable c)
;;
