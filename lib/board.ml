open Base
open Consumables

type consumable =
  | Tarot of Tarot.t
  | Planet of Planet.t
  | Spectral of Spectral.t

type 'a interval = {
  min: 'a;
  max: 'a;
}

type t = {
  deck: Deck.t;
  draw: Card.t list;
  jokers: Joker.t list;
  consumables: consumable list;
  draw_size: int;
  max_jokers: int;
  max_consumables: int;
  hand_size: int interval;
}

let deck { deck } = deck
let draw { draw } = draw

let create color =
  let deck = Deck.create color in
  {
    deck;
    draw = [];
    jokers = [];
    consumables = [];
    draw_size = 1;
    max_jokers = 5;
    max_consumables = 2;
    hand_size = { min = 1; max = 5 };
  }
;;

let fill_draw board =
  let rec loop board size =
    if size <= 0 then
      board
    else (
      let sample, rest = board |> deck |> Deck.cards |> Util.draw in
      let board =
        { board with deck = Deck.set_cards rest board.deck; draw = sample :: board.draw }
      in
      loop board (size - 1)
    )
  in
  loop board (board.draw_size - List.length board.draw)
;;
