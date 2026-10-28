exception Invalid_sample_position

let split_at n lst =
  let rec loop n rest acc =
    if n <= 0 then (List.rev acc, rest)
    else
      match rest with
      | [] -> (List.rev acc, [])
      | x :: xs -> loop (n - 1) xs (x :: acc)
  in
  loop n lst []

let draw lst =
  let n = List.length lst in
  let pos = Random.int n in
  let head, tail = split_at pos lst in
  match tail with
  | [] -> raise Invalid_sample_position
  | sample :: rest -> (sample, head @ rest)

let list_of_enum min_enum max_enum element_of_enum =
  let indices = Range.from min_enum max_enum in
  Range.fold_right [] indices ~f:(fun acc indice ->
      match element_of_enum indice with
      | Some element -> element :: acc
      | None -> acc)

let groups (type k) ~(key : 'a -> k) (xs : 'a list) =
  let module M = Map.Make (struct
    type t = k

    let compare = Stdlib.compare
  end) in
  List.fold_left
    (fun acc x ->
      let k = key x in
      M.update k (function None -> Some [ x ] | Some ys -> Some (x :: ys)) acc)
    M.empty xs
  |> M.bindings
  |> List.map (fun (k, ys) -> (k, List.rev ys))
