type record = {
  hand: Poker_hand.t;
  level: int;
  scoring_effect: Score.t;
}

val score : Consumables.Planet.t -> Poker_hand.t -> Score.change list
val consume : Consumables.Planet.t -> record -> record
val base_hands : (Poker_hand.t * record) list
