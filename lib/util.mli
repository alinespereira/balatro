val split_at : int -> 'a list -> 'a list * 'a list
val draw : Random.State.t -> 'a list -> 'a * 'a list
val shuffle : Random.State.t -> 'a list -> 'a list
val groups : key:('a -> 'b) -> 'a list -> ('b * 'a list) list
