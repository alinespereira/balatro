type t

type suit
type rank



val create : suit -> rank -> t
val suit : t -> suit
val rank : t -> rank

val all_suits : suit list
val all_ranks : rank list