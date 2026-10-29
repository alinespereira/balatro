type t
type suit [@@deriving show, eq, enum, ord]
type rank [@@deriving show, eq, enum, ord]

val create : suit -> rank -> t
val suit : t -> suit
val rank : t -> rank
val all_suits : suit list
val all_ranks : rank list

module RankMap : Map.S with type key = rank
module SuitMap : Map.S with type key = suit
