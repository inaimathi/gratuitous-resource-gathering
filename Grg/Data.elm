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
technologies = let tech (name, upgrade, reqs) = 
                       {name=name, cost=[], upgrade=upgrade, reqs=reqs, limit=One}
                   struct (name, cost, upgrade, reqs, reps) =
                       {name=name, cost=cost, upgrade=upgrade, reqs=reqs, limit=reps}
               in concat 
                  [map tech [ ("Basic Stone Tools", [Income "Food" 5], [Res "Stone" 5])
                            , ("Fire", [Income "Food" 5], [Res "Stone" 5, Res "Wood" 10])
                            , ("Stone Tools", [Income "Food" 5], [Res "Stone" 15, Tech "Basic Stone Tools"])
                            , ("Language", [Capacity "Workers" 100], [Res "Workers" 25])
                            , ("Ceramics", [Capacity "Food" 50], [Res "Clay" 25])
                            , ("Domestication", [Capacity "Food" 50], [Res "Food" 100])
                            , ("Hunting Implements", [Income "Food" 5, Capacity "Food" 50], [Res "Wood" 50])
                            , ("Copper Tools", [Income "Food" 5, Capacity "Food" 10], [Tech "Copper Mine", Tech "Fire", Tech "Stone Tools"])
                            , ("Agriculture", [Income "Food" 10, Capacity "Food" 50], [Res "Wood" 50, Tech "Domestication", Res "Food" 50])
                            , ("The Wheel", [Income "Food" 10, Capacity "Workers" 10], [Res "Wood" 50, Tech "Agriculture", Res "Knowledge" 50])
                            , ("Writing", [Income "Knowledge" 5, Capacity "Knowledge" 500], [Res "Workers" 200])
                            , ("Bronze Tools", [Income "Food" 5, Capacity "Food" 10], [Tech "Fire", Tech "Bronze Mine"])
                            , ("Salt", [Capacity "Food" 10], [Tech "Bronze Tools"])
                              
                            , ("Sundial", [], [Res "Knowledge" 250, Tech "Stone Tools"])
                            , ("Glass", [], [Res "Knowledge" 250, Tech "Fire"])
                            , ("Optics", [], [Res "Knowledge" 500, Tech "Glass"])
                            , ("Philosophy", [Income "Knowledge" 10, Capacity "Knowledge" 500], [Tech "Writing", Res "Knowledge" 500, Res "Workers" 250])
                            , ("Mathematics", [], [Tech "Philosophy", Res "Knowledge" 500])
                            , ("Construction", [], [Tech "Mathematics", Tech "Bronze Tools"])
                            , ("Iron Tools", [], [Tech "Bronze Tools"])
                              
                            , ("Engineering", [], [Tech "Mathematics", Tech "Construction"])
                            , ("Metal Casting", [], [Tech "Iron Tools"])
                            , ("Education", [Income "Knowledge" 10, Capacity "Knowledge" 500], [Tech "Mathematics", Tech "Philosophy", Res "Knowledge" 750])
                            , ("Machinery", [], [Tech "Engineering"])
                            , ("Physics", [], [Tech "Mathematics"])
                            , ("Steel", [], [Tech "Engineering", Tech "Metal Casting"])
                              
                            , ("Astronomy", [], [Tech "Optics", Tech "Engineering", Tech "Physics"])
                            , ("Acoustics", [], [Tech "Engineering", Tech "Machinery", Tech "Physics"])
                            , ("Printing Press", [Income "Knowledge" 10, Capacity "Knowledge" 500], [Tech "Engineering"])
                            , ("Banking", [], [Tech "Printing Press"])
                              
                            -- , ("Navigation", [], [])
                            -- , ("Chemistry", [], [])
                            -- , ("Metallurgy", [], [])
                            -- , ("Scientific Theory", [], [])
                            -- , ("Fertilizer", [], [])
                            -- , ("Biology", [], [])
                            -- , ("Steam Power", [], [])
                              
                            -- , ("Electricity", [], [])
                            -- , ("Railroad", [], [])
                              
                            -- , ("Refrigiration", [], [])
                            -- , ("Telegraph", [], [])
                            -- , ("Radio", [], [])
                            -- , ("Flight", [], [])
                            -- , ("Combustion", [], [])
                              
                            -- , ("Penicilin", [], [])
                            -- , ("Plastics", [], [])
                            -- , ("Electronics", [], [])
                            -- , ("Mass Media", [], [])
                              
                            -- , ("Radar", [], [])
                            -- , ("Atomic Theory", [], [])
                            -- , ("Ecology", [], [])
                              
                            -- , ("Digital Computers", [], [])
                            -- , ("Rocketry", [], [])
                            -- , ("Lasers", [], [])
                            -- , ("Nuclear Fission", [], [])
                            -- , ("Robotics", [], [])
                            -- , ("Satellites", [], [])
                              
                            -- , ("Particle Physics", [], [])
                            -- , ("Nuclear Fusion", [], [])
                            -- , ("Nanotechnology", [], [])
                            , ("P=NP", [], [Tech "Mathematics", Tech "Artificial Intelligence", Res "Knowledge" 25000])
                            , ("Artificial Intelligence", [], [Tech "Digital Computers"])
                            , ("Mind Uploading", [], [Tech "Nanotechnology", Tech "Digital Computers", Tech "Biology"])
                            , ("The Singularity", [], [Tech "P=NP", Tech "Artificial Intelligence", Tech "Mind Uploading"])
                            ],
                   
                  map struct [ ("Clay Pit", [("Wood", 30)], [Income "Clay" 3], [], Infinity)
                             , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 3, Capacity "Wood" 10], [], Infinity)
                             , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 3, Capacity "Workers" 5], [], Infinity)
                             , ("Granary", [("Wood", 40), ("Stone", 30)], [Capacity "Food" 15], [Tech "Farm"], Infinity)
                             , ("Dwelling", [("Wood", 20), ("Stone", 20)], [Capacity "Workers" 2], [Tech "Farm"], Infinity)
                             , ("Pottery", [("Wood", 25), ("Clay", 25)], [Capacity "Food" 10], [Tech "Clay Pit"], Infinity)
                             , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 3], [Tech "Lumber Yard"], Infinity)
                             , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"], Infinity)
                             , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"], Infinity)
                             , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [Income "Iron" 1], [Tech "Mine"], Infinity)
                             ]]
                      

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
