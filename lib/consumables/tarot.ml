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

(* TODO: one case per implemented tarot (see the .mli) and [(0, 0)] for the rest. *)
let selection _tarot = failwith "TODO: Tarot.selection"

(* TODO: [List.map] fits most tarots. [Death] needs exactly two cards: a pattern like
   [[left; right]] takes them apart. *)
let change_cards _tarot _cards = failwith "TODO: Tarot.change_cards"

(* TODO: [TheHermit] gives [min money 20] more; the others change nothing. *)
let change_money _tarot _money = failwith "TODO: Tarot.change_money"
