type t =
  | TheFool
  | TheMagician
  | TheHighPriestess
  | TheEmpress
  | TheEmperor
  | TheHierophant
  | TheLovers
  | TheChariot
  | Justice
  | TheHermit
  | TheWheelOfFortune
  | Strength
  | TheHangedMan
  | Death
  | Temperance
  | TheDevil
  | TheTower
  | TheStar
  | TheMoon
  | TheSun
  | Judgement
  | TheWorld
[@@deriving show, eq, ord, enum, enumerate]

let implemented =
  [TheHermit; Strength; TheHangedMan; Death; TheStar; TheMoon; TheSun; TheWorld]
;;

let selection = function
  | Strength
  | TheHangedMan ->
    1, 2
  | Death -> 2, 2
  | TheStar
  | TheMoon
  | TheSun
  | TheWorld ->
    1, 3
  | TheHermit
  | _ ->
    0, 0
;;

let change_cards tarot (cards : Card.t list) =
  let to_suit suit cards = List.map (fun card -> Some (Card.with_suit suit card)) cards in
  match tarot, cards with
  | Strength, _ ->
    List.map
      (fun card ->
         let { Card.rank; _ } = card in
         Some (Card.with_rank (Card.next_rank rank) card))
      cards
  | TheHangedMan, _ -> List.map (Fun.const None) cards
  | TheStar, _ -> to_suit Card.Diamonds cards
  | TheMoon, _ -> to_suit Card.Clubs cards
  | TheSun, _ -> to_suit Card.Hearts cards
  | TheWorld, _ -> to_suit Card.Spades cards
  | Death, [_; right] -> [Some right; Some right]
  | Death, _ -> invalid_arg "Tarot.change_cards: Death needs 2 cards"
  | TheHermit, _
  | _ ->
    List.map Option.some cards
;;

let change_money tarot money =
  match tarot with
  | TheHermit -> money + Int.min money 20
  | Strength
  | TheHangedMan
  | TheStar
  | TheMoon
  | TheSun
  | TheWorld
  | Death
  | _ ->
    money
;;
