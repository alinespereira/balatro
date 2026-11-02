open Balatro
open Balatro_cli

let wait () = Unix.sleepf 1.5

let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout
;;

(* What the player sees before each prompt: the blind and the hand while playing, the
   offers while shopping. The jokers and the consumables show in both. *)
let screen (run : Run.t) =
  let jokers = run |> Run.board |> Board.jokers |> Render.jokers in
  let consumables = run |> Run.board |> Board.consumables |> Render.consumables in
  match Run.phase run with
  | Run.Playing ->
    [
      Render.status run; jokers; consumables; run |> Run.board |> Board.hand |> Render.hand;
    ]
  | Shopping { shop; _ } ->
    [Render.money (Run.money run); jokers; consumables; Render.shop shop]
;;

let rec loop ?(help = false) rng (run : Run.t) : unit =
  if not help then wait ();
  clear_screen ();
  if help then print_endline Command.help;
  List.iter print_endline (screen run);
  print_string "> ";
  flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    (match Command.parse line with
     | Error e ->
       print_endline (Command.error_message e);
       loop rng run
     | Ok Command.Help -> loop rng run ~help:true
     | Ok Command.Quit -> ()
     | Ok (Command.Act action) ->
       (match Action.apply rng action run with
        | Error e ->
          print_endline (Render.error e);
          loop rng run
        | Ok (Action.Done, next) -> loop rng next
        | Ok (Action.Played (play, points), next) ->
          print_endline (Render.play play points);
          loop rng next
        | Ok (Action.BlindWon (play, points, payout), next) ->
          print_endline (Render.play play points);
          print_endline "Blind vencido!";
          print_endline (Render.payout payout);
          loop rng next
        | Ok (Action.RunWon (play, points), _) ->
          print_endline (Render.play play points);
          print_endline "Você venceu!"
        | Ok (Action.RunLost (play, points), _) ->
          print_endline (Render.play play points);
          print_endline "Você perdeu!"))
;;

let () =
  let rng = Random.State.make_self_init () in
  let run = Run.create rng Deck.Red in

  loop rng run ~help:true
;;
