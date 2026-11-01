open Balatro_cli

let parsed =
  Alcotest.(
    result
      (testable Command.pp Command.equal)
      (testable Command.pp_error Command.equal_error))
;;

(* One row per line typed by the player: the line and what [parse] should return. *)
let cases : (string * (Command.t, Command.error) result) list =
  [
    "p 1 3 5", Ok (Command.Play [0; 2; 4]);
    "d 2 4", Ok (Command.Discard [1; 3]);
    "  p   1    2  ", Ok (Command.Play [0; 1]);
    "p\t1\t2", Ok (Command.Play [0; 1]);
    "p", Ok (Command.Play []);
    "d", Ok (Command.Discard []);
    "p 0", Error (Command.NotAnIndex "0");
    "p 1 -2", Error (Command.NotAnIndex "-2");
    "p 1 x", Error (Command.NotAnIndex "x");
    "c 1", Ok (Command.Buy 0);
    "v 2", Ok (Command.Sell 1);
    "c", Error (Command.ExpectedOneIndex "c");
    "v 1 2", Error (Command.ExpectedOneIndex "v");
    "c 0", Error (Command.NotAnIndex "0");
    "r", Ok Command.Reroll;
    "s", Ok Command.Leave;
    "h", Ok Command.Help;
    "?", Ok Command.Help;
    "q", Ok Command.Quit;
    "h 3", Error (Command.ArgsProvided ("h", ["3"]));
    "q agora", Error (Command.ArgsProvided ("q", ["agora"]));
    "", Error Command.Empty;
    "   ", Error Command.Empty;
    "jogar 1", Error (Command.UnknownCommand "jogar");
    "P 1", Error (Command.UnknownCommand "P");
  ]
;;

let test_parse () =
  List.iter
    (fun (line, expected) ->
       Alcotest.check parsed (Printf.sprintf "%S" line) expected (Command.parse line))
    cases
;;

let test_help_mentions_every_command () =
  List.iter
    (fun cmd ->
       let mentioned =
         String.split_on_char '\n' Command.help
         |> List.exists (fun line ->
           match String.split_on_char ' ' (String.trim line) with
           | first :: _ when first = cmd -> true
           | _ :: "ou" :: second :: _ when second = cmd -> true
           | _ -> false)
       in
       Alcotest.(check bool) cmd true mentioned)
    ["p"; "d"; "h"; "?"; "q"; "c"; "v"; "r"; "s"]
;;

let test_error_message () =
  Alcotest.(check string)
    "unknown command"
    "Comando inválido: 'x'"
    (Command.error_message (Command.UnknownCommand "x"));
  Alcotest.(check string)
    "args provided"
    "Comando 'h' não aceita argumentos, mas recebeu '1 2'"
    (Command.error_message (Command.ArgsProvided ("h", ["1"; "2"])))
;;

let () =
  Alcotest.run
    "Command"
    [
      ( "command",
        [
          Alcotest.test_case "parse" `Quick test_parse;
          Alcotest.test_case
            "help mentions every command"
            `Quick
            test_help_mentions_every_command;
          Alcotest.test_case "error message" `Quick test_error_message;
        ] );
    ]
;;
