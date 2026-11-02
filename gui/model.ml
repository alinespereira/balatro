type t = {
  run: Balatro.Run.t;
  selected: int list;
  message: string option;
}

let create run = { run; selected = []; message = None }
