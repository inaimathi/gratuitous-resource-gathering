module Grg where

import Graphics.Input as In
import Dict as D
import Maybe as M

-- The Model
data Upgrade = Income String Int | Capacity String Int
data Prerequisite = Tech String | Res String Int
type Resource = { name:String, reqs:[Prerequisite] }
type Technology = { name:String, cost:[(String, Int)], upgrade:[Upgrade], reqs:[Prerequisite] }

technologies = let tech (name, cost, upgrade, reqs) = {name=name, cost=cost, upgrade=upgrade, reqs=reqs}
               in map tech
                      [ ("Clay Pit", [("Wood", 30)], [Income "Clay" 1], [])
                      , ("Pottery", [("Wood", 25), ("Clay", 25)], [], [Tech "Clay Pit"])
                      , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 1], [])
                      , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 1], [Tech "Lumber Yard"])
                      , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 1], [])
                      , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"])
                      , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"])
                      , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [], [Tech "Mine"])
                      ]

resources = let res (name, reqs) = {name=name, reqs=reqs}
            in map res
                   [ ("Wood", []), ("Stone", []),  ("Food", []), ("Clay", [])
                   , ("Ore", [Tech "Mine"]), ("Salt", [Tech "Mine"]), ("Gold", [Tech "Gold Mine"])
                   , ("Money", [Tech "Printing Press", Tech "Mint"]), ("Reputation", [Tech "Printing Press"])
                   , ("Trade", [Tech "Shipyard"]), ("Credit", [Tech "Banking"]), ("Knowledge", [Tech "Philosophy"])
                   ]

type GameState = { skill: D.Dict String Int
                 , income: [(String, Int)]
                 , capacity: D.Dict String Int
                 , built:[String]
                 , balance: D.Dict String Int
                 }

defaultGame : GameState
defaultGame = 
    { skill = D.empty
    , income = []
    , capacity = D.fromList [("Wood", 50), ("Stone", 100), ("Food", 50), ("Salt", 50), ("Clay", 50)]
    , built = []
    , balance = D.fromList [("Workers", 2), ("Food", 10)]
    }

meetsPrereqs : GameState -> [Prerequisite] -> Bool
meetsPrereqs g reqs = let meets prereq = case prereq of
                                           Tech name -> any (\b -> b == name) g.built
                                           Res name amt -> amt < D.findWithDefault 0 name g.balance
                      in and <| map meets reqs

updateWithDefault : a -> comparable -> (a -> a) -> D.Dict comparable a -> D.Dict comparable a
updateWithDefault def name fn dict = D.insert name (fn <| D.findWithDefault def name dict) dict

incrementDict d table = let inc d (name, amt) = updateWithDefault 0 name ((+) amt) d
                            recur d table = if isEmpty table then d else inc d <| head table
                        in recur d table

tick : GameState -> GameState
tick g = { g | balance <- incrementDict g.balance g.income }

gather : GameState -> String -> GameState
gather g res = let increment = 1 + D.findWithDefault 0 res g.skill
               in { g | balance <- incrementDict g.balance [(res, increment)] } 

build : GameState -> String -> GameState
build g tech = g

applyEvent : GameState -> Event -> GameState
applyEvent g ev = case ev of
                    TechClick n -> build g n
                    ResClick n -> gather g n
                    Tick -> tick g

-- The Inputs
data Event = TechClick String | ResClick String | Tick
printEvent ev = case ev of
                  TechClick n -> "UPGRADE " ++ n
                  ResClick n -> "GATHERED " ++ n
                  Tick -> "TICK"

techGroup = In.buttons <| TechClick ""
resourceGroup = In.buttons <| ResClick ""
clock = sampleOn (every second) <| constant Tick

events = merges [techGroup.events, resourceGroup.events, clock]

techButtons g = map (\t -> techGroup.button (TechClick t.name) t.name) <| filter (meetsPrereqs g . .reqs) technologies
resourceButtons g = map (\r -> resourceGroup.button (ResClick r.name) r.name) <| filter (meetsPrereqs g . .reqs) resources

-- The Game
game = foldp (\val memo -> applyEvent memo val) defaultGame events

-- The View
showGame : GameState -> Element
showGame g = flow down [ flow right <| resourceButtons g
                       , asText <| D.toList g.balance
                       , flow right <| techButtons g ]

main = lift showGame game

port log : Signal String
port log = lift printEvent events
