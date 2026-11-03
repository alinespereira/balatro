let size = 24

(* raylib only draws the characters its font was loaded with: ASCII, the accented
   letters of Latin-1 and the four suits. *)
let codepoints =
  List.init 95 (fun i -> 32 + i)
  @ List.init 96 (fun i -> 160 + i)
  @ [0x2660; 0x2663; 0x2665; 0x2666]
;;

let load () =
  Raylib.load_font_from_memory
    ".ttf"
    Font_data.contents
    size
    (Ctypes.CArray.of_list Ctypes.int codepoints)
;;
