(** How the model changes with each click. Nothing here calls raylib: [Main] reads the
    mouse and turns a click into an {!event}. *)

type event = Click of float * float (** A left click at that point of the window. *)

(** Selects the card at that position, or unselects it if it was selected. A card past
    [Balatro.Board.max_selected] is not selected: the list stays as it was. *)
val toggle : int -> int list -> int list

(** What to tell the player after an action: the hand and its points, the blind won and
    what it paid, or the end of the run. [None] for [Done]. *)
val message : Balatro.Action.outcome -> string option

(** The model after the event.

    While playing, a click on a card {!toggle}s it, and a click on [Layout.play_button]
    or [Layout.discard_button] runs [Action.Play] or [Action.Discard] on the selected
    cards, in ascending order. That empties the selection and sets the {!message}; if
    the run refused, the selection stays and the message is [Label.error] of the error.

    While shopping (the shop comes later), any click leaves the shop. Once the run is
    won or lost, clicks change nothing. *)
val step : Random.State.t -> event -> Model.t -> Model.t
