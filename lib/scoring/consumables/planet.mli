module PlanetMap : Map.S with type key = Base.Consumables.Planet.t

val scores : Base.Effect.change list PlanetMap.t
