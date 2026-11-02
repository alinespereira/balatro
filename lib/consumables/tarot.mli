(** Tarot cards: consumables that change cards of the hand or give money. They wait in
    a consumable slot of the board until the player uses them. *)

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

(** The tarots that already do something, in the order of [all]. The shop only offers
    these:

    - [TheStar], [TheMoon], [TheSun] and [TheWorld]: up to 3 cards become ♦, ♣, ♥ and ♠;
    - [Strength]: up to 2 cards go up one rank ([Card.next_rank]);
    - [TheHangedMan]: destroys up to 2 cards;
    - [Death]: of 2 cards, the left one becomes a copy of the right one;
    - [TheHermit]: doubles the money, giving at most $20. *)
val implemented : t list

(** How many cards of the hand the tarot needs, as [(min, max)]: [(1, 3)] for the suit
    tarots, [(1, 2)] for [Strength] and [TheHangedMan], [(2, 2)] for [Death], and
    [(0, 0)] for [TheHermit] and for the tarots that are not [implemented]. *)
val selection : t -> int * int

(** What the tarot does to the selected cards, given in the order they are in the
    hand. The answer has one entry per card, in the same order: [Some card] for what
    the card became (maybe the same card), [None] for a card that was destroyed. A
    tarot that does not change cards gives every card back unchanged. *)
val change_cards : t -> Card.t list -> Card.t option list

(** The money after using the tarot: [TheHermit] adds as much as there is, up to $20.
    The other tarots leave it as it is. *)
val change_money : t -> int -> int
