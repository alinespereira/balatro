open Balatro

let wait () = Unix.sleepf 1.5

let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout
;;

let rec loop ?(help = false) rng (run : Run.t) : unit =
  wait ();
  clear_screen ();
  if help then print_endline Command.help;
  print_endline (Render.status run);
  print_endline (run |> Run.blind |> Blind.board |> Board.jokers |> Render.jokers);
  print_endline (run |> Run.blind |> Blind.board |> Board.hand |> Render.hand);
  print_string "> ";
  flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    (match Command.parse line with
     | Error e ->
       print_endline (Command.error_message e);
       loop rng run
     | Ok Command.Help ->
       print_endline Command.help;
       loop rng run
     | Ok Command.Quit -> ()
     | Ok (Command.Play indices) ->
       (match Run.play indices run with
        | Error e ->
          print_endline (Render.error e);
          loop rng run
        | Ok (play, points, new_run) ->
          print_endline (Render.play play points);
          (match Run.status new_run with
           | InProgress when Blind.status (Run.blind new_run) = Blind.Won ->
             (match Run.next_blind rng new_run with
              | Ok (payout, next) ->
                print_endline "Blind vencido!";
                print_endline (Render.payout payout);
                loop rng next
              | Error e -> print_endline (Render.error e))
           | InProgress -> loop rng new_run
           | Won -> print_endline "Você venceu!"
           | Lost -> print_endline "Você perdeu!"))
     | Ok (Command.Discard indices) ->
       (match Run.discard indices run with
        | Error e ->
          print_endline (Render.error e);
          loop rng run
        | Ok new_run -> loop rng new_run))
;;

let () =
  let rng = Random.State.make_self_init () in
  let run = Run.create rng Deck.Red in

  loop rng run ~help:true
;;
