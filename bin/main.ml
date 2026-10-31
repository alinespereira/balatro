open Balatro

(* TODO: print the status and the hand, read a line with In_channel.input_line,
   parse it with Command.parse and act on it:
   - Play: call Run.play, show the result, then check Run.status and
     Blind.status (Run.blind run) to know whether to call Run.next_blind;
   - Discard: call Run.discard;
   - Help: print Command.help;
   - Quit, or end of input: stop.
   Call [loop] again with the new run until the run is Won or Lost; it will need
   [rec] then. *)
let rec loop rng (run : Run.t) : unit =
  print_endline (Render.status run);
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
     | Ok Command.Help -> print_endline Command.help
     | Ok Command.Quit -> ()
     | Ok (Command.Play indices) ->
       (match Run.play indices run with
        | Error e -> print_endline (Render.error e)
        | Ok (play, points, new_run) ->
          print_endline (Render.play play points);
          (match Run.status new_run with
           | InProgress -> loop rng new_run
           | Won -> print_endline "You Won"
           | Lost -> print_endline "You Lost"))
     | Ok (Command.Discard indices) ->
       (match Run.discard indices run with
        | Error e -> print_endline (Render.error e)
        | Ok new_run -> loop rng new_run))
;;

let () =
  let rng = Random.State.make_self_init () in
  let run = Run.create rng Deck.Red in
  print_endline Command.help;
  loop rng run
;;
