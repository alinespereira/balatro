open Base.Effect
open Base.Consumables.Planet
module PlanetMap = Map.Make (Base.Consumables.Planet)

let scores =
  PlanetMap.empty
  |> PlanetMap.add Pluto [ AddChips 10; AddMult 1 ]
  |> PlanetMap.add Mercury [ AddChips 15; AddMult 1 ]
  |> PlanetMap.add Uranus [ AddChips 20; AddMult 1 ]
  |> PlanetMap.add Venus [ AddChips 20; AddMult 2 ]
  |> PlanetMap.add Saturn [ AddChips 30; AddMult 3 ]
  |> PlanetMap.add Jupiter [ AddChips 15; AddMult 2 ]
  |> PlanetMap.add Earth [ AddChips 25; AddMult 2 ]
  |> PlanetMap.add Mars [ AddChips 30; AddMult 3 ]
  |> PlanetMap.add Neptune [ AddChips 40; AddMult 4 ]
