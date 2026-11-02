open Balatro
open Balatro_text

let test_card () =
  Alcotest.(check string) "king" "K♥" (Label.card (Card.create Card.Hearts Card.King));
  Alcotest.(check string) "ten" "10♠" (Label.card (Card.create Card.Spades Card.Ten))
;;

let test_stage () =
  Alcotest.(check string) "small" "Small Blind" (Label.stage Run.Small);
  Alcotest.(check string) "big" "Big Blind" (Label.stage Run.Big);
  Alcotest.(check string) "boss" "Boss Blind" (Label.stage Run.Boss)
;;

let test_hand_name () =
  Alcotest.(check string) "pair" "Par" (Label.hand_name Poker.Hand.Pair);
  Alcotest.(check string) "two pair" "Dois pares" (Label.hand_name Poker.Hand.TwoPair);
  (* Every hand gets its own name, and none of them is the constructor. *)
  let names = List.map Label.hand_name Poker.Hand.all in
  Alcotest.(check int)
    "names are distinct"
    (List.length names)
    (List.length (List.sort_uniq String.compare names));
  List.iter
    (fun hand ->
       Alcotest.(check bool)
         (Poker.Hand.show hand)
         false
         (Label.hand_name hand = Poker.Hand.show hand))
    Poker.Hand.all
;;

let distinct strings = List.length (List.sort_uniq String.compare strings)

(* Whether [sub] appears somewhere in [s]. *)
let contains ~sub s =
  let n = String.length sub in
  let rec from i = i + n <= String.length s && (String.sub s i n = sub || from (i + 1)) in
  from 0
;;

let test_boss () =
  let eclipses, others =
    List.partition
      (function
        | Boss.Eclipse _ -> true
        | _ -> false)
      Boss.all
  in
  (* Every eclipse has the same name, and every other boss a name of its own. *)
  Alcotest.(check int)
    "one name for the eclipses"
    1
    (distinct (List.map Label.boss_name eclipses));
  Alcotest.(check int)
    "a name for each of the others"
    (List.length others + 1)
    (distinct (List.map Label.boss_name (List.hd eclipses :: others)));
  (* Every boss, each eclipse included, says what it does in its own words. *)
  Alcotest.(check int)
    "a description for each boss"
    (List.length Boss.all)
    (distinct (List.map Label.boss_description Boss.all));
  Alcotest.(check bool)
    "the eclipse names its suit"
    true
    (contains ~sub:"♦" (Label.boss_description (Boss.Eclipse Card.Diamonds)))
;;

let () =
  Alcotest.run
    "Label"
    [
      ( "label",
        [
          Alcotest.test_case "card" `Quick test_card;
          Alcotest.test_case "stage" `Quick test_stage;
          Alcotest.test_case "boss" `Quick test_boss;
          Alcotest.test_case "hand name" `Quick test_hand_name;
        ] );
    ]
;;
