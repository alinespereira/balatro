(** The cards drawn with raylib shapes (circles, triangles, rounded rectangles), with
    no image files. Every position is in pixels, with [y] growing downwards. *)

open Balatro

(** The size of a card, in pixels. *)
val width : float

val height : float

(** The colour of the ink of a suit: red for hearts and diamonds, dark for the others. *)
val suit_color : Card.suit -> Raylib.Color.t

(** [draw_suit suit ~center ~size color] draws the symbol of [suit] around [center].
    [size] is about half the width of the symbol, so the same function draws the small
    symbol in the corner and the big one in the middle. *)
val draw_suit
  :  Card.suit ->
  center:Raylib.Vector2.t ->
  size:float ->
  Raylib.Color.t ->
  unit

(** The face of a card, with its top-left corner at [at]. *)
val draw_front : Raylib.Font.t -> Card.t -> at:Raylib.Vector2.t -> unit

(** The back of the cards of a deck, with its top-left corner at [at]. *)
val draw_back : Deck.color -> at:Raylib.Vector2.t -> unit
