(* The drawing board: every suit, a few cards and the three backs, side by side.
   Run it with [dune exec balatro-art] after each change. [S] saves a picture. *)

open Balatro
open Balatro_art

let width = 1100
let height = 520
let background = Raylib.Color.create 28 74 52 255
let gap = 14.

let cards =
  [
    Card.create Spades Ace;
    Card.create Hearts Seven;
    Card.create Diamonds Ten;
    Card.create Clubs King;
  ]
;;

let decks = Deck.[Red; Blue; Green]

(* The x of the [i]-th card of the row. *)
let column i = gap +. (float_of_int i *. (Card_art.width +. gap))

let draw font =
  Raylib.clear_background background;
  List.iteri
    (fun i suit ->
       let center = Raylib.Vector2.create (column i +. (Card_art.width /. 2.)) 90. in
       Card_art.draw_suit suit ~center ~size:50. (Card_art.suit_color suit))
    Card.all_of_suit;
  List.iteri
    (fun i card ->
       Card_art.draw_front font card ~at:(Raylib.Vector2.create (column i) 190.))
    cards;
  List.iteri
    (fun i color ->
       Card_art.draw_back color ~at:(Raylib.Vector2.create (column (i + 4)) 190.))
    decks;
  Raylib.draw_text_ex
    font
    "S: salvar imagem   Esc: sair"
    (Raylib.Vector2.create gap (float_of_int height -. 40.))
    (float_of_int Typeface.size)
    1.
    Raylib.Color.raywhite
;;

let rec frame font =
  if not (Raylib.window_should_close ()) then (
    if Raylib.is_key_pressed Raylib.Key.S then Raylib.take_screenshot "cartas.png";
    Raylib.begin_drawing ();
    draw font;
    Raylib.end_drawing ();
    frame font
  )
;;

let () =
  (* Smooth edges on circles and triangles. *)
  Raylib.set_config_flags Raylib.ConfigFlags.msaa_4x_hint;
  Raylib.init_window width height "Balatro: as cartas";
  Raylib.set_target_fps 60;
  let font = Typeface.load () in
  frame font;
  Raylib.close_window ()
;;
