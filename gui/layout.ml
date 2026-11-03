type rect = {
  x: float;
  y: float;
  width: float;
  height: float;
}

let width = 960
let height = 540
let card_width = 80.
let card_height = 112.
let gap = 12.
let selected_lift = 24.

let contains { x = rx; y = ry; width; height } (x, y) =
  rx <= x && x <= rx +. width && ry <= y && y <= ry +. height
;;

let cards ~selected n =
  let total = (float_of_int n *. card_width) +. (float_of_int (n - 1) *. gap) in
  let left = (float_of_int width -. total) /. 2. in
  let top = float_of_int height -. card_height -. 100. in
  List.init n (fun i ->
    let lift =
      if List.mem i selected then
        selected_lift
      else
        0.
    in
    {
      x = left +. (float_of_int i *. (card_width +. gap));
      y = top -. lift;
      width = card_width;
      height = card_height;
    })
;;

(* TODO(Aline): look for the first rectangle of [cards ~selected n] that [contains]
   the point, and return its position. [List.find_index] may help. *)
let card_at ~selected:(_ : int list) (_ : int) (_ : float * float) : int option =
  failwith "TODO: Layout.card_at"
;;

let button_width = 160.
let button_height = 48.
let button_top = float_of_int height -. 76.

let play_button =
  {
    x = (float_of_int width /. 2.) -. button_width -. 10.;
    y = button_top;
    width = button_width;
    height = button_height;
  }
;;

let discard_button =
  {
    x = (float_of_int width /. 2.) +. 10.;
    y = button_top;
    width = button_width;
    height = button_height;
  }
;;
