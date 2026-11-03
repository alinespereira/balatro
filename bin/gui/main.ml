(* The first window: it shows the state of a new run, the same words as the terminal,
   and does nothing yet when clicked. *)

open Balatro
open Balatro_text
open Balatro_cli
open Balatro_art

let width = 960
let height = 540
let line_height = 32.

let draw_lines font ~x ~y lines =
  List.iteri
    (fun i line ->
       let position = Raylib.Vector2.create x (y +. (float_of_int i *. line_height)) in
       Raylib.draw_text_ex
         font
         line
         position
         (float_of_int Typeface.size)
         1.
         Raylib.Color.raywhite)
    lines
;;

let draw font run =
  Raylib.clear_background Raylib.Color.darkgreen;
  draw_lines font ~x:24. ~y:24. (String.split_on_char '\n' (Render.status run));
  let hand = run |> Run.board |> Board.hand |> List.map Label.card in
  draw_lines font ~x:24. ~y:(float_of_int height -. 56.) [String.concat "   " hand]
;;

let rec frame font run =
  if not (Raylib.window_should_close ()) then (
    Raylib.begin_drawing ();
    draw font run;
    Raylib.end_drawing ();
    frame font run
  )
;;

let () =
  Raylib.init_window width height "Balatro";
  Raylib.set_target_fps 60;
  let font = Typeface.load () in
  let rng = Random.State.make_self_init () in
  frame font (Run.create rng Deck.Red);
  Raylib.close_window ()
;;
