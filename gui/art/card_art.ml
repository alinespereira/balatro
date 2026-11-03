open Balatro
open Raylib

let width = 140.
let height = 196.

(* The palette. Every drawing takes its colours from here, so changing one line here
   changes it on every card. The last number is the opacity: 255 is solid. *)
let ink = Color.create 32 34 48 255
let paper = Color.create 247 242 232 255
let red = Color.create 222 72 64 255
let unfinished = Color.create 190 186 178 255

let suit_color : Card.suit -> Color.t = function
  | Hearts
  | Diamonds ->
    red
  | Clubs
  | Spades ->
    ink
;;

let deck_color : Deck.color -> Color.t = function
  | Red -> Color.create 214 70 64 255
  | Blue -> Color.create 64 112 204 255
  | Green -> Color.create 60 160 96 255
;;

(* [point center dx dy] is the point [dx] to the right of [center] and [dy] below it.
   Drawing around a center keeps every shape in proportion with [size]. *)
let point center dx dy = Vector2.create (Vector2.x center +. dx) (Vector2.y center +. dy)

(* raylib only fills a triangle whose corners come counter-clockwise on the screen;
   given the other way round, it draws nothing. This one checks the order with a cross
   product and swaps two corners when needed, so the corners can come in any order. *)
let triangle a b c color =
  let open Vector2 in
  let cross = ((x b -. x a) *. (y c -. y a)) -. ((y b -. y a) *. (x c -. x a)) in
  if cross < 0. then
    draw_triangle a b c color
  else
    draw_triangle a c b color
;;

(* A grey circle with a cross, for the suits that do not have a drawing yet. *)
let placeholder ~center ~size =
  draw_circle_v center size unfinished;
  let line dx dy =
    draw_line_ex (point center (-.dx) (-.dy)) (point center dx dy) 3. paper
  in
  line (size *. 0.5) (size *. 0.5);
  line (size *. 0.5) (-.size *. 0.5)
;;

(* An example to start from: a diamond is two triangles, one pointing up and one
   pointing down, joined at the middle. *)
let draw_diamond ~center ~size color =
  let top = point center 0. (-.size *. 1.2) in
  let bottom = point center 0. (size *. 1.2) in
  let left = point center (-.size *. 0.85) 0. in
  let right = point center (size *. 0.85) 0. in
  triangle top left right color;
  triangle left bottom right color
;;

(* Your turn: draw_circle_v and triangle are enough for the other three. *)
let draw_suit (suit : Card.suit) ~center ~size color =
  match suit with
  | Diamonds -> draw_diamond ~center ~size color
  | Hearts
  | Spades
  | Clubs ->
    placeholder ~center ~size
;;

(* A rounded rectangle: [roundness] goes from 0 (square corners) to 1 (a pill). *)
let rounded ~at ~w ~h color =
  let rect = Rectangle.create (Vector2.x at) (Vector2.y at) w h in
  draw_rectangle_rounded rect 0.15 12 color
;;

(* A dark rounded rectangle with a smaller one on top makes the border. *)
let frame ~at fill =
  rounded ~at ~w:width ~h:height ink;
  rounded ~at:(point at 3. 3.) ~w:(width -. 6.) ~h:(height -. 6.) fill
;;

let draw_front font (card : Card.t) ~at =
  let color = suit_color card.suit in
  frame ~at paper;
  let text_size = float_of_int Typeface.size in
  draw_text_ex
    font
    (Balatro_text.Label.rank card.rank)
    (point at 12. 8.)
    text_size
    1.
    color;
  draw_suit card.suit ~center:(point at 21. 48.) ~size:8. color;
  draw_suit card.suit ~center:(point at (width /. 2.) (height /. 2.)) ~size:26. color
;;

(* Your turn too: only the colour of the deck, for now. Stripes, triangles, circles... *)
let draw_back color ~at = frame ~at (deck_color color)
