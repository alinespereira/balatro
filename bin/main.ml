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
let loop _rng (_ : Run.t) : unit = failwith "TODO: loop"

let () =
  let rng = Random.State.make_self_init () in
  let run = Run.create rng Deck.Red in
  print_endline Command.help;
  loop rng run
