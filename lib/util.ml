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
