(* The first window: it shows the state of a new run, the same words as the terminal,
   and does nothing yet when clicked. *)

open Balatro
open Balatro_text
open Balatro_cli

let width = 960
let height = 540
let font_size = 24
let line_height = 32.

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

let draw_lines font ~x ~y lines =
  List.iteri
    (fun i line ->
       let position = Raylib.Vector2.create x (y +. (float_of_int i *. line_height)) in
       Raylib.draw_text_ex
         font
         line
         position
         (float_of_int font_size)
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
  let font = load_font () in
  let rng = Random.State.make_self_init () in
  frame font (Run.create rng Deck.Red);
  Raylib.close_window ()
;;
