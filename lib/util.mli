val split_at : int -> 'a list -> 'a list * 'a list
val draw : 'a list -> 'a * 'a list
val list_of_enum : Range.elt -> Range.elt -> (int -> 'a option) -> 'a list
val groups : key:('a -> 'b) -> 'a list -> ('b * 'a list) list
