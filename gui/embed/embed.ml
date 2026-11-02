(* Prints an OCaml module that holds the bytes of the file named on the command line:
   [let contents = "..."]. *)

let () =
  let contents = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  Printf.printf "let contents = %S\n" contents
;;
