(* The window: it reads the mouse, hands each click to [Update.step] and draws the
   model. Everything that decides something lives in [Balatro_gui]; this file only
   talks to raylib. *)

open Balatro
open Balatro_text
open Balatro_cli
open Balatro_gui

let font_size = 20
let line_height = 26.

(* raylib only draws the characters its font was loaded with: ASCII, the accented
   letters of Latin-1 and the four suits. *)
let codepoints =
  List.init 95 (fun i -> 32 + i)
  @ List.init 96 (fun i -> 160 + i)
  @ [0x2660; 0x2663; 0x2665; 0x2666]
;;

(* Needs the window: raylib uploads the font to the graphics card. *)
let load_font () =
  Raylib.load_font_from_memory
    ".ttf"
    Font_data.contents
    font_size
    (Ctypes.CArray.of_list Ctypes.int codepoints)
;;

let text font line ~x ~y color =
  Raylib.draw_text_ex
    font
    line
    (Raylib.Vector2.create x y)
    (float_of_int font_size)
    1.
    color
;;

let draw_lines font ~x ~y lines =
  List.iteri
    (fun i line ->
       text font line ~x ~y:(y +. (float_of_int i *. line_height)) Raylib.Color.raywhite)
    lines
;;

let rectangle (r : Layout.rect) =
  Raylib.Rectangle.create r.Layout.x r.Layout.y r.Layout.width r.Layout.height
;;

let suit_color (card : Card.t) =
  match card.Card.suit with
  | Card.Hearts
  | Card.Diamonds ->
    Raylib.Color.red
  | Card.Clubs
  | Card.Spades ->
    Raylib.Color.black
;;

let draw_card font (r : Layout.rect) card ~selected =
  Raylib.draw_rectangle_rec (rectangle r) Raylib.Color.raywhite;
  if selected then
    Raylib.draw_rectangle_lines_ex (rectangle r) 4. Raylib.Color.gold
  else
    Raylib.draw_rectangle_lines_ex (rectangle r) 1. Raylib.Color.darkgray;
  text font (Label.card card) ~x:(r.Layout.x +. 10.) ~y:(r.Layout.y +. 8.) (suit_color card)
;;

let draw_button font (r : Layout.rect) label =
  Raylib.draw_rectangle_rec (rectangle r) Raylib.Color.darkblue;
  text font label ~x:(r.Layout.x +. 16.) ~y:(r.Layout.y +. 12.) Raylib.Color.raywhite
;;

let draw font (model : Model.t) =
  Raylib.clear_background Raylib.Color.darkgreen;
  draw_lines font ~x:24. ~y:24. (String.split_on_char '\n' (Render.status model.Model.run));
  Option.iter
    (fun message -> text font message ~x:520. ~y:24. Raylib.Color.gold)
    model.Model.message;
  match Run.phase model.Model.run with
  | Run.Playing ->
    let hand = Board.hand (Run.board model.Model.run) in
    let places = Layout.cards ~selected:model.Model.selected (List.length hand) in
    List.iteri
      (fun i (card, r) -> draw_card font r card ~selected:(List.mem i model.Model.selected))
      (List.combine hand places);
    draw_button font Layout.play_button "Jogar";
    draw_button font Layout.discard_button "Descartar"
  | Run.Shopping _ ->
    text font "Loja (em breve): clique para seguir" ~x:24. ~y:320. Raylib.Color.raywhite
;;

(* The only place that asks raylib about the mouse. *)
let read_event () =
  if Raylib.is_mouse_button_pressed Raylib.MouseButton.Left then (
    let point = Raylib.get_mouse_position () in
    Some (Update.Click (Raylib.Vector2.x point, Raylib.Vector2.y point))
  ) else
    None
;;

let rec frame rng font model =
  if not (Raylib.window_should_close ()) then (
    let model =
      match read_event () with
      | Some event -> Update.step rng event model
      | None -> model
    in
    Raylib.begin_drawing ();
    draw font model;
    Raylib.end_drawing ();
    frame rng font model
  )
;;

let () =
  Raylib.init_window Layout.width Layout.height "Balatro";
  Raylib.set_target_fps 60;
  let font = load_font () in
  let rng = Random.State.make_self_init () in
  frame rng font (Model.create (Run.create rng Deck.Red));
  Raylib.close_window ()
;;
