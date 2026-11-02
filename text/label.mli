(** The words the player sees for each thing in the game, in Portuguese. They do not
    depend on where they are shown: the terminal uses them through [Render], and a
    graphical interface can use the same ones. *)

(** A suit as its symbol, such as ["♥"]. *)
val suit : Balatro.Card.suit -> string

(** A rank as it is written on the card, such as ["10"] or ["K"]. *)
val rank : Balatro.Card.rank -> string

(** A card in two or three characters, such as ["K♥"] or ["10♠"]. *)
val card : Balatro.Card.t -> string

(** The name of a joker, such as ["Coringa Alegre"]. *)
val joker_name : Balatro.Joker.t -> string

(** What the joker does. For a joker with state, it starts with what the joker gives
    now, such as ["+3 mult; ..."]. *)
val joker_description : Balatro.Joker.t -> string

(** The name of a planet, such as ["Plutão"]. *)
val planet_name : Balatro.Consumables.Planet.t -> string

(** The name of a tarot, such as ["A Estrela"]. *)
val tarot_name : Balatro.Consumables.Tarot.t -> string

(** What the tarot does. *)
val tarot_description : Balatro.Consumables.Tarot.t -> string

(** The name of a boss, such as ["A Seca"]. *)
val boss_name : Balatro.Boss.t -> string

(** What the boss does, such as ["sem descartes"]. *)
val boss_description : Balatro.Boss.t -> string

(** The name of a blind stage, such as ["Small Blind"]. *)
val stage : Balatro.Run.stage -> string

(** The name of a poker hand, such as ["Dois pares"]. *)
val hand_name : Balatro.Poker.Hand.t -> string

(** A consumable and what it does, such as ["Plutão: sobe o nível de Carta alta"]. *)
val consumable : Balatro.Consumables.Consumable.t -> string

(** A shop offer and what it does, without its price. *)
val item : Balatro.Shop.item -> string

(** A message for an error returned by any function of [Run]. *)
val error : Balatro.Run.error -> string
