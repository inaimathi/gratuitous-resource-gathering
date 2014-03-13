module Grg.Data where

import Dict as D

data Limit = Count Int | DependsOn (GameState -> Int) | One | Infinity
type Price = [(String, Int)]
data Upgrade = Income String Int | Capacity String Int | UpgradeFn (GameState -> GameState)
data Prerequisite = Tech String | Res String Int | GameFn (GameState -> Bool)
type Resource = { name:String, reqs:[Prerequisite], upkeep:[(String, Float)] }
type Technology = { name:String, cost:Price, upgrade:[Upgrade], reqs:[Prerequisite], limit:Limit }

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
                      [ ("Language", [], [Capacity "Workers" 100], [Res "Workers" 10], One)
                      , ("Stone Tools", [], [Income "Food" 5], [Res "Stone" 10], One)

--                      , ("Clay Pit", [("Wood", 30)], [Income "Clay" 3], [], Infinity)
--                      , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 3, Capacity "Wood" 10], [], Infinity)
--                      , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 3, Capacity "Workers" 5], [], Infinity)
                      , ("Granary", [("Wood", 40), ("Stone", 30)], [Capacity "Food" 15], [Tech "Farm"], Infinity)
                      , ("Dwelling", [("Wood", 20), ("Stone", 20)], [Capacity "Workers" 2], [Tech "Farm"], Infinity)
                      , ("Pottery", [("Wood", 25), ("Clay", 25)], [Capacity "Food" 10], [Tech "Clay Pit"], Infinity)
                      , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 3], [Tech "Lumber Yard"], Infinity)
                      , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"], Infinity)
                      , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"], Infinity)
                      , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [Income "Iron" 1], [Tech "Mine"], Infinity) -- Upgrade here
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
