open Balatro
open Balatro_text

let hand cards =
  let sep = "|" in
  let cards_row =
    List.map
      (fun { Card.suit = s; Card.rank = r } ->
         Printf.sprintf "  %2s%s " (Label.rank r) (Label.suit s))
      cards
    |> String.concat sep
  in
  let indices_row =
    List.mapi (fun i _ -> Printf.sprintf "  %3d " (i + 1)) cards |> String.concat sep
  in
  let divider_row = String.make (String.length indices_row) '-' in
  String.concat "\n" [indices_row; divider_row; cards_row]
;;

let jokers = function
  | [] -> ""
  | jokers ->
    jokers
    |> List.mapi (fun i joker ->
      Printf.sprintf
        "  %d. %s: %s"
        (i + 1)
        (Label.joker_name joker)
        (Label.joker_description joker))
    |> String.concat "\n"
;;

let money amount = Printf.sprintf "Dinheiro: $%d" amount

let consumables = function
  | [] -> ""
  | consumables ->
    consumables
    |> List.mapi (fun i c -> Printf.sprintf "  %d. %s" (i + 1) (Label.consumable c))
    |> String.concat "\n"
;;

let shop (the_shop : Shop.t) =
  let offers =
    the_shop.offers
    |> List.mapi (fun i offer ->
      Printf.sprintf "  %d. $%d  %s" (i + 1) (Shop.price offer) (Label.item offer))
  in
  String.concat
    "\n"
    (("Loja:" :: offers) @ [Printf.sprintf "  Rerrolar: $%d" the_shop.reroll_cost])
;;

let status run =
  let { Run.ante; Run.stage = current; Run.boss; Run.blind; Run.money = cash; _ } = run in
  let { Blind.target; Blind.plays_left; Blind.discards_left; Blind.chips; _ } = blind in
  let boss_label =
    match current with
    | Run.Boss -> "Chefe"
    | Small
    | Big ->
      "Chefe do ante"
  in
  String.concat
    "\n"
    [
      Printf.sprintf "Ante: %d" ante;
      Printf.sprintf "Rodada: %s" (Label.stage current);
      Printf.sprintf
        "%s: %s (%s)"
        boss_label
        (Label.boss_name boss)
        (Label.boss_description boss);
      Printf.sprintf "Meta: %d" target;
      "";
      Printf.sprintf "Fichas: %d" chips;
      Printf.sprintf "Jogadas: %d" plays_left;
      Printf.sprintf "Descartes: %d" discards_left;
      money cash;
    ]
;;

let play play points =
  String.concat
    "\n"
    [
      Printf.sprintf "Mão: %s" (Label.hand_name (Play.hand play));
      Printf.sprintf "Pontos: %d" points;
    ]
;;

let payout payout =
  let { Payout.blind; Payout.plays; Payout.interest } = payout in
  String.concat
    "\n"
    [
      Printf.sprintf "Recompensa do blind: $%d" blind;
      Printf.sprintf "Jogadas restantes: $%d" plays;
      Printf.sprintf "Juros: $%d" interest;
      Printf.sprintf "Total: $%d" (Payout.total payout);
    ]
;;
