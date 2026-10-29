open Balatro

(* A card is written as its rank followed by its suit, so "TH" is the ten of hearts and
   "AS" the ace of spades. A hand is cards separated by spaces. *)

let rank_of_char = function
  | '2' -> Card.Two
  | '3' -> Card.Three
  | '4' -> Card.Four
  | '5' -> Card.Five
  | '6' -> Card.Six
  | '7' -> Card.Seven
  | '8' -> Card.Eight
  | '9' -> Card.Nine
  | 'T' -> Card.Ten
  | 'J' -> Card.Jack
  | 'Q' -> Card.Queen
  | 'K' -> Card.King
  | 'A' -> Card.Ace
  | c -> invalid_arg (Printf.sprintf "unknown rank %c" c)
;;

let suit_of_char = function
  | 'C' -> Card.Clubs
  | 'D' -> Card.Diamonds
  | 'H' -> Card.Hearts
  | 'S' -> Card.Spades
  | c -> invalid_arg (Printf.sprintf "unknown suit %c" c)
;;

let card s = Card.create (suit_of_char s.[1]) (rank_of_char s.[0])
let hand s = String.split_on_char ' ' s |> List.filter (( <> ) "") |> List.map card

(* Each example is paired with the category it should be detected as, or [None] when it
   is not a valid hand to play (Balatro plays from 1 to 5 cards).

   Every [is_*] check answers "is this exactly the hand's category?". So a royal flush is
   not a straight flush, a full house of one suit is a flush house, and a pair is not a
   high card. *)
let examples =
  Poker.Hand.
    [
      "single card", "AS", Some HighCard;
      "five unrelated cards", "2C 7D 9H JS KC", Some HighCard;
      "four cards in sequence", "2C 3D 4H 5S", Some HighCard;
      "straight wrapping around the ace", "QC KD AH 2S 3C", Some HighCard;
      "four cards of one suit", "2H 5H 9H JH", Some HighCard;
      "four of one suit and one other", "2H 5H 9H JH KC", Some HighCard;
      "pair alone", "KS KH", Some Pair;
      "pair and a kicker", "QC QD 3H", Some Pair;
      "pair and three kickers", "2C 2D 5H 9S JC", Some Pair;
      "two pairs alone", "3C 3D 8H 8S", Some TwoPair;
      "two pairs and a kicker", "2C 2D 5H 5S JC", Some TwoPair;
      "three of a kind alone", "7C 7D 7H", Some ThreeOfAKind;
      "three of a kind and a kicker", "7C 7D 7H 2S", Some ThreeOfAKind;
      "three of a kind and two kickers", "7C 7D 7H 2S KC", Some ThreeOfAKind;
      "straight", "2C 3D 4H 5S 6C", Some Straight;
      "straight out of order", "6C 2D 5H 3S 4C", Some Straight;
      "straight with a low ace", "AC 2D 3H 4S 5C", Some Straight;
      "straight with a high ace", "TC JD QH KS AC", Some Straight;
      "flush", "2H 5H 9H JH KH", Some Flush;
      "flush with a pair", "2H 2H 9H JH KH", Some Flush;
      "full house", "2C 2D 2H 3S 3C", Some FullHouse;
      "full house out of order", "3C 2D 2H 3S 2C", Some FullHouse;
      "four of a kind alone", "9C 9D 9H 9S", Some FourOfAKind;
      "four of a kind and a kicker", "9C 9D 9H 9S KC", Some FourOfAKind;
      "straight flush", "5S 6S 7S 8S 9S", Some StraightFlush;
      "straight flush with a low ace", "AD 2D 3D 4D 5D", Some StraightFlush;
      "royal flush", "TH JH QH KH AH", Some RoyalFlush;
      "five of a kind", "AS AS AH AD AC", Some FiveOfAKind;
      "flush house", "4C 4C 4C 8C 8C", Some FlushHouse;
      "flush five", "6D 6D 6D 6D 6D", Some FlushFive;
      "no cards", "", None;
      "six cards", "2C 2D 2H 3S 3C 3H", None;
    ]
;;

let detectors =
  Poker.Hand.
    [
      HighCard, is_high_card;
      Pair, is_pair;
      TwoPair, is_two_pair;
      ThreeOfAKind, is_three_of_a_kind;
      Straight, is_straight;
      Flush, is_flush;
      FullHouse, is_full_house;
      FourOfAKind, is_four_of_a_kind;
      StraightFlush, is_straight_flush;
      RoyalFlush, is_royal_flush;
      FiveOfAKind, is_five_of_a_kind;
      FlushHouse, is_flush_house;
      FlushFive, is_flush_five;
    ]
;;

(* Runs every detector on every example and reports all failures before exiting, so one
   broken detector does not hide the others. *)
let () =
  let failures = ref 0 in
  List.iter
    (fun (category, detect) ->
       List.iter
         (fun (name, cards, expected_category) ->
            let expected = expected_category = Some category in
            let fail message =
              incr failures;
              Printf.printf
                "FAIL %s, %s [%s]: %s\n"
                (Poker.Hand.show category)
                name
                cards
                message
            in
            match detect (hand cards) with
            | actual when actual = expected -> ()
            | actual -> fail (Printf.sprintf "expected %b, got %b" expected actual)
            | exception Failure message -> fail ("raised " ^ message))
         examples)
    detectors;
  if !failures > 0 then (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  )
;;
