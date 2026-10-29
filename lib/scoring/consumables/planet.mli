module PlanetMap : Map.S with type key = Base.Consumables.Planet.t

val scores : Score.change list PlanetMap.t
