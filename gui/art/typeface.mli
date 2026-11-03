(** The font of the game, DejaVu Sans, carried inside the executable. *)

(** The size the font is loaded at, in pixels. Text drawn at this size is the
    sharpest. *)
val size : int

(** Loads the font with the accented letters and the four suits. It needs the window
    open: raylib uploads the font to the graphics card. *)
val load : unit -> Raylib.Font.t
