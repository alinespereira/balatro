(** Where each thing sits in the window, in pixels from its top-left corner. Nothing
    here draws: [Draw] uses these rectangles to paint, and [Update] uses them to know
    what a click hit. *)

type rect = {
  x: float;
  y: float;
  width: float;
  height: float;
}

(** The size of the window. *)
val width : int

val height : int

(** Whether the point [(x, y)] is inside the rectangle, edges included. *)
val contains : rect -> float * float -> bool

(** The rectangles of a hand of [n] cards, left to right, centred at the bottom of the
    window. A card whose position is in [selected] sits [selected_lift] pixels higher. *)
val cards : selected:int list -> int -> rect list

(** How much higher a selected card sits. *)
val selected_lift : float

(** The position (0-based) of the card under the point in a hand of [n] cards, with
    the selected ones raised as {!cards} places them, or [None] if no card is there. *)
val card_at : selected:int list -> int -> float * float -> int option

(** The two buttons, under the hand. *)
val play_button : rect

val discard_button : rect
