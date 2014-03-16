module Grg.Data where

import Dict as D

data GameVal = Count Int | DependsOn (GameState -> Int) | One | Infinity
type Price = [(String, Int)]
data Upgrade = Income String Int | Capacity String Int | UpgradeFn (GameState -> GameState)
data Prerequisite = Tech String | Res String Int | GameFn (GameState -> Bool)
type Resource = { name:String, reqs:[Prerequisite], upkeep:[(String, Float)] }
type Technology = { name:String, cost:Price, upgrade:[Upgrade], reqs:[Prerequisite], limit:GameVal }

data Event = TechClick String | ResClick String | Assign String | Free String | Tick

type GameState = { skill: D.Dict String Int
                 , income: D.Dict String Int
                 , workers: D.Dict String Int
                 , capacity: D.Dict String Int
                 , built: D.Dict String Int
                 , balance: D.Dict String Int }

techTable = D.fromList <| map (\t -> (t.name, t)) technologies
technologies = let tech (name, cost, upgrade, reqs, reps) = 
                       {name=name, cost=cost, upgrade=upgrade, reqs=reqs, limit=reps}
               in map tech
                      [ ("Basic Stone Tools", [], [Income "Food" 5], [Res "Stone" 5], One)
                      , ("Fire", [], [Income "Food" 5], [Res "Stone" 5, Res "Wood" 10], One)
                      , ("Stone Tools", [], [Income "Food" 5], [Res "Stone" 15], One)
                      , ("Language", [], [Capacity "Workers" 100], [Res "Workers" 15], One)
                      , ("Ceramics", [], [Capacity "Food" 50], [Res "Clay" 25], One)
                      , ("Domestication", [], [Capacity "Food" 50], [Res "Food" 100], One)
                      , ("Hunting Implements", [], [Income "Food" 5, Capacity "Food" 50], [Res "Wood" 50], One)
                      , ("Copper Tools", [], [Income "Food" 5, Capacity "Food" 10], [Tech "Copper Mine", Tech "Fire"], One)
                      , ("Agriculture", [], [Income "Food" 10, Capacity "Food" 50], [Res "Wood" 50, Tech "Domestication", Res "Food" 50], One)
--                      , ("The Wheel", [], [], [], One)
                      , ("Writing", [], [Income "Knowledge" 5, Capacity "Knowledge" 500], [Res "Workers" 200], One)
                      , ("Bronze Tools", [], [Income "Food" 5, Capacity "Food" 10], [Tech "Bronze Tools", Tech "Bronze Mine"], One)
                      , ("Salt", [], [Capacity "Food" 10], [Tech "Bronze Tools"], One)

                      -- , ("Sundial", [], [], [], One)
                      -- , ("Glass", [], [], [], One)
                      -- , ("Optics", [], [], [], One)
                      -- , ("Philosophy", [], [], [], One)
                      -- , ("Mathematics", [], [], [], One)
                      -- , ("Construction", [], [], [], One)
                      -- , ("Iron Tools", [], [], [], One)

                      -- , ("Engineering", [], [], [], One)
                      -- , ("Metal Casting", [], [], [], One)
                      -- , ("Education", [], [], [], One)
                      -- , ("Machinery", [], [], [], One)
                      -- , ("Physics", [], [], [], One)
                      -- , ("Steel", [], [], [], One)

                      -- , ("Astronomy", [], [], [], One)
                      -- , ("Acoustics", [], [], [], One)
                      -- , ("Printing Press", [], [], [], One)
                      -- , ("Banking", [], [], [], One)

                      -- , ("Navigation", [], [], [], One)
                      -- , ("Chemistry", [], [], [], One)
                      -- , ("Metallurgy", [], [], [], One)
                      -- , ("Scientific Theory", [], [], [], One)
                      -- , ("Fertilizer", [], [], [], One)
                      -- , ("Biology", [], [], [], One)
                      -- , ("Steam Power", [], [], [], One)

                      -- , ("Electricity", [], [], [], One)
                      -- , ("Railroad", [], [], [], One)

                      -- , ("Refrigiration", [], [], [], One)
                      -- , ("Telegraph", [], [], [], One)
                      -- , ("Radio", [], [], [], One)
                      -- , ("Flight", [], [], [], One)
                      -- , ("Combustion", [], [], [], One)

                      -- , ("Penicilin", [], [], [], One)
                      -- , ("Plastics", [], [], [], One)
                      -- , ("Electronics", [], [], [], One)
                      -- , ("Mass Media", [], [], [], One)

                      -- , ("Radar", [], [], [], One)
                      -- , ("Atomic Theory", [], [], [], One)
                      -- , ("Ecology", [], [], [], One)

                      -- , ("Digital Computers", [], [], [], One)
                      -- , ("Rocketry", [], [], [], One)
                      -- , ("Lasers", [], [], [], One)
                      -- , ("Nuclear Fission", [], [], [], One)
                      -- , ("Robotics", [], [], [], One)
                      -- , ("Satellites", [], [], [], One)

                      -- , ("Particle Physics", [], [], [], One)
                      -- , ("Nuclear Fusion", [], [], [], One)
                      -- , ("Nanotechnology", [], [], [], One)
                      -- , ("P=NP", [], [], [], One)
                      -- , ("Artificial Intelligence", [], [], [], One)
                      -- , ("Mind Uploading", [], [], [], One)                        
                      -- , ("The Singularity", [], [], [], One)


--                      , ("Clay Pit", [("Wood", 30)], [Income "Clay" 3], [], Infinity)
--                      , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 3, Capacity "Wood" 10], [], Infinity)
--                      , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 3, Capacity "Workers" 5], [], Infinity)
--                      , ("Granary", [("Wood", 40), ("Stone", 30)], [Capacity "Food" 15], [Tech "Farm"], Infinity)
--                      , ("Dwelling", [("Wood", 20), ("Stone", 20)], [Capacity "Workers" 2], [Tech "Farm"], Infinity)
--                      , ("Pottery", [("Wood", 25), ("Clay", 25)], [Capacity "Food" 10], [Tech "Clay Pit"], Infinity)
--                      , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 3], [Tech "Lumber Yard"], Infinity)
--                      , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"], Infinity)
--                      , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"], Infinity)
--                      , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [Income "Iron" 1], [Tech "Mine"], Infinity) -- Upgrade here
                      ]

resTable = D.fromList <| map (\r -> (r.name, r)) resources
resources = let base name = {name=name, reqs=[], upkeep=[]}
                reqd (name, reqs) = {name=name, reqs=reqs, upkeep=[]}
                res (name, reqs, upkeep) = {name=name, reqs=reqs, upkeep=upkeep}
            in map res [ ("Food", [], [("Salt", 1/6)])
                       , ("Workers", [], [("Food", 1)])

                       , ("Knowledge", [Tech "Language"], [])
                         
                       , ("Clay", [], []), ("Wood", [], []), ("Stone", [], [])
                       , ("Ore", [Tech "Mine"], []), ("Salt", [Tech "Mine"], []), ("Gold", [Tech "Gold Mine"], [])
                         
                       , ("Money", [Tech "Printing Press", Tech "Mint"], []) 
                       , ("Reputation", [Tech "Printing Press"], [])
                       , ("Trade", [Tech "Shipyard"], [])
                       ]
