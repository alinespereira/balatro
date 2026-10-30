type t  (* cartas jogadas + categoria + cartas que pontuam *)
val make : Card.t list -> t
val score : t -> int