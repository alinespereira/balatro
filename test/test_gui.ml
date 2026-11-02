open Balatro
open Balatro_text
open Balatro_gui

let rng () = Random.State.make [| 42 |]

let center (r : Layout.rect) =
  r.Layout.x +. (r.Layout.width /. 2.), r.Layout.y +. (r.Layout.height /. 2.)
;;

let click_on r = Update.Click (fst (center r), snd (center r))

(* The click on the card at that position of a full hand, none of it selected. *)
let click_card ?(selected = []) idx = click_on (List.nth (Layout.cards ~selected 8) idx)

(* A point where there is nothing: the top-right corner of the window. *)
let nowhere = Update.Click (float_of_int Layout.width -. 1., 1.)

let test_contains () =
  let r = { Layout.x = 10.; y = 20.; width = 30.; height = 40. } in
  Alcotest.(check bool) "inside" true (Layout.contains r (25., 40.));
  Alcotest.(check bool) "top-left corner" true (Layout.contains r (10., 20.));
  Alcotest.(check bool) "bottom-right corner" true (Layout.contains r (40., 60.));
  Alcotest.(check bool) "left of it" false (Layout.contains r (9., 40.));
  Alcotest.(check bool) "below it" false (Layout.contains r (25., 61.))
;;

let test_card_at () =
  let cards = Layout.cards ~selected:[] 8 in
  List.iteri
    (fun i r ->
       Alcotest.(check (option int))
         (Printf.sprintf "card %d" i)
         (Some i)
         (Layout.card_at ~selected:[] 8 (center r)))
    cards;
  let first = List.hd cards in
  Alcotest.(check (option int))
    "the gap after the first card"
    None
    (Layout.card_at
       ~selected:[]
       8
       (first.Layout.x +. first.Layout.width +. 1., first.Layout.y +. 1.));
  (* A selected card is higher: its top edge is hit, and the strip it left is not. *)
  let raised = List.nth (Layout.cards ~selected:[2] 8) 2 in
  let below = raised.Layout.y +. raised.Layout.height +. (Layout.selected_lift /. 2.) in
  Alcotest.(check (option int))
    "top of a raised card"
    (Some 2)
    (Layout.card_at ~selected:[2] 8 (raised.Layout.x +. 1., raised.Layout.y +. 1.));
  Alcotest.(check (option int))
    "under a raised card"
    None
    (Layout.card_at ~selected:[2] 8 (raised.Layout.x +. 1., below))
;;

let test_toggle () =
  Alcotest.(check (list int)) "select" [3] (Update.toggle 3 []);
  Alcotest.(check (list int)) "unselect" [1] (List.sort compare (Update.toggle 3 [3; 1]));
  let full = List.init Board.max_selected Fun.id in
  Alcotest.(check (list int)) "no room for one more" full (Update.toggle 7 full);
  Alcotest.(check int)
    "but one can still be unselected"
    (Board.max_selected - 1)
    (List.length (Update.toggle 0 full))
;;

let test_message () =
  let play =
    match
      Play.make [Card.create Card.Hearts Card.King; Card.create Card.Spades Card.King]
    with
    | Ok play -> play
    | Error _ -> Alcotest.fail "two kings are a valid play"
  in
  let says what outcome =
    match Update.message outcome with
    | Some text -> Alcotest.(check bool) what true (String.length text > 0)
    | None -> Alcotest.fail (what ^ ": no message")
  in
  says "played" (Action.Played (play, 42));
  says
    "blind won"
    (Action.BlindWon (play, 42, Payout.make ~reward:3 ~plays_left:2 ~money:4));
  says "run won" (Action.RunWon (play, 42));
  says "run lost" (Action.RunLost (play, 42));
  Alcotest.(check (option string)) "done" None (Update.message Action.Done)
;;

let chips (model : Model.t) = Blind.chips (Run.blind model.Model.run)

let test_select_and_play () =
  let rng = rng () in
  let impossible ~ante:_ _ = max_int in
  let model = Model.create (Run.create ~target:impossible rng Deck.Red) in
  let model = Update.step rng (click_card 0) model in
  Alcotest.(check (list int)) "selected" [0] model.Model.selected;
  let again = Update.step rng (click_card ~selected:[0] 0) model in
  Alcotest.(check (list int)) "unselected" [] again.Model.selected;
  let missed = Update.step rng nowhere model in
  Alcotest.(check (list int)) "a click on nothing" [0] missed.Model.selected;
  let played = Update.step rng (click_on Layout.play_button) model in
  Alcotest.(check (list int)) "selection emptied" [] played.Model.selected;
  Alcotest.(check bool) "chips scored" true (chips played > 0);
  Alcotest.(check bool) "a message" true (Option.is_some played.Model.message)
;;

let test_refused_play () =
  let rng = rng () in
  let model = Model.create (Run.create rng Deck.Red) in
  let refused = Update.step rng (click_on Layout.play_button) model in
  Alcotest.(check int) "no chips" 0 (chips refused);
  Alcotest.(check (option string))
    "the error"
    (Some (Label.error (Run.BlindError (Blind.BoardError Board.NoCards))))
    refused.Model.message
;;

let test_discard () =
  let rng = rng () in
  let model = Model.create (Run.create rng Deck.Red) in
  let model = Update.step rng (click_card 0) model in
  let model = Update.step rng (click_card ~selected:[0] 1) model in
  let discarded = Update.step rng (click_on Layout.discard_button) model in
  Alcotest.(check (list int)) "selection emptied" [] discarded.Model.selected;
  Alcotest.(check int)
    "a discard spent"
    (Blind.discards_left (Run.blind model.Model.run) - 1)
    (Blind.discards_left (Run.blind discarded.Model.run))
;;

let test_shop () =
  let rng = rng () in
  let easy ~ante:_ _ = 1 in
  let model = Model.create (Run.create ~target:easy rng Deck.Red) in
  let model = Update.step rng (click_card 0) model in
  let won = Update.step rng (click_on Layout.play_button) model in
  let in_shop (m : Model.t) =
    match Run.phase m.Model.run with
    | Run.Shopping _ -> true
    | Run.Playing -> false
  in
  Alcotest.(check bool) "in the shop after the blind" true (in_shop won);
  Alcotest.(check bool) "any click leaves" false (in_shop (Update.step rng nowhere won))
;;

let () =
  Alcotest.run
    "gui"
    [
      ( "layout",
        [
          Alcotest.test_case "contains" `Quick test_contains;
          Alcotest.test_case "card at" `Quick test_card_at;
        ] );
      ( "update",
        [
          Alcotest.test_case "toggle" `Quick test_toggle;
          Alcotest.test_case "message" `Quick test_message;
          Alcotest.test_case "select and play" `Quick test_select_and_play;
          Alcotest.test_case "refused play" `Quick test_refused_play;
          Alcotest.test_case "discard" `Quick test_discard;
          Alcotest.test_case "shop" `Quick test_shop;
        ] );
    ]
;;
