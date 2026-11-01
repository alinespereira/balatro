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

let money amount = Printf.sprintf "Dinheiro: $%d" amount

(* The hands a planet levels up: Neptune is the only one with two. *)
let planet_hands planet =
  Poker.Hand.all
  |> List.filter (fun hand -> Poker.Hand.planet hand = planet)
  |> List.map hand_name
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
  let { Run.ante; Run.stage = current; Run.boss; Run.blind; Run.money = cash; _ } = run in
  let { Blind.target; Blind.plays_left; Blind.discards_left; Blind.chips; _ } = blind in
  String.concat
    "\n"
    [
      Printf.sprintf "Ante: %d" ante;
      Printf.sprintf "Rodada: %s" (stage current);
      (* TODO: "Chefe do ante: ..." during the Small and Big blinds, when the boss is
         only a preview, and "Chefe: ..." during the Boss blind. *)
      Printf.sprintf "Chefe: %s (%s)" (boss_name boss) (boss_description boss);
      Printf.sprintf "Meta: %d" target;
      "";
      Printf.sprintf "Fichas: %d" chips;
      Printf.sprintf "Jogadas: %d" plays_left;
      Printf.sprintf "Descartes: %d" discards_left;
      money cash;
    ]
;;

let play play points =
  String.concat
    "\n"
    [
      Printf.sprintf "Mão: %s" (hand_name (Play.hand play));
      Printf.sprintf "Pontos: %d" points;
    ]
;;

let payout payout =
  let { Payout.blind; Payout.plays; Payout.interest } = payout in
  String.concat
    "\n"
    [
      Printf.sprintf "Recompensa do blind: $%d" blind;
      Printf.sprintf "Jogadas restantes: $%d" plays;
      Printf.sprintf "Juros: $%d" interest;
      Printf.sprintf "Total: $%d" (Payout.total payout);
    ]
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
