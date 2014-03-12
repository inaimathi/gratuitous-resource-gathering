module Grg.Data where

import Dict as D

data Reps = Count Int | One | Inf
type Price = [(String, Int)]
data Upgrade = Income String Int | Capacity String Int
data Prerequisite = Tech String | Res String Int
type Resource = { name:String, reqs:[Prerequisite], upkeep:[(String, Float)] }
type Technology = { name:String, cost:Price, upgrade:[Upgrade], reqs:[Prerequisite], repeatable:Reps }

data Event = TechClick String | ResClick String | Assign String | Free String | Tick

type GameState = { skill: D.Dict String Int
                 , income: D.Dict String Int
                 , workers: D.Dict String Int
                 , capacity: D.Dict String Int
                 , built: D.Dict String Int
                 , balance: D.Dict String Int }

techTable = D.fromList <| map (\t -> (t.name, t)) technologies
technologies = let tech (name, cost, upgrade, reqs, reps) = 
                       {name=name, cost=cost, upgrade=upgrade, reqs=reqs, repeatable=reps}
               in map tech
                      [ ("Clay Pit", [("Wood", 30)], [Income "Clay" 3], [], Inf)
                      , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 3, Capacity "Wood" 10], [], Inf)
                      , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 3, Capacity "Workers" 5], [], Inf)
                      , ("Granary", [("Wood", 40), ("Stone", 30)], [Capacity "Food" 15], [Tech "Farm"], Inf)
                      , ("Dwelling", [("Wood", 20), ("Stone", 20)], [Capacity "Workers" 2], [Tech "Farm"], Inf)
                      , ("Pottery", [("Wood", 25), ("Clay", 25)], [Capacity "Food" 10], [Tech "Clay Pit"], Inf)
                      , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 3], [Tech "Lumber Yard"], Inf)
                      , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"], Inf)
                      , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"], Inf)
                      , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [Income "Iron" 1], [Tech "Mine"], Inf) -- Upgrade here
                      , ("Language", [], [Capacity "Workers" 100], [Res "Workers" 10], One)]

resTable = D.fromList <| map (\r -> (r.name, r)) resources
resources = let base name = {name=name, reqs=[], upkeep=[]}
                reqd (name, reqs) = {name=name, reqs=reqs, upkeep=[]}
                uppd (name, reqs, upkeep) = {name=name, reqs=reqs, upkeep=upkeep}
            in concat [ map base ["Wood", "Stone", "Clay"]
                      , map reqd [ ("Ore", [Tech "Mine"]), ("Salt", [Tech "Mine"]), ("Gold", [Tech "Gold Mine"])
                                 , ("Money", [Tech "Printing Press", Tech "Mint"]), ("Reputation", [Tech "Printing Press"])
                                 , ("Trade", [Tech "Shipyard"]), ("Credit", [Tech "Banking"]), ("Knowledge", [Tech "Philosophy", Tech "Speech"])]
                      , map uppd [ ("Food", [], [("Salt", 1/6)])
                                 , ("Workers", [Tech "Dwelling"], [("Food", 1)])]]
