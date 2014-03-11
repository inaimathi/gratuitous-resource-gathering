module Grg where

import Graphics.Input as In
import Graphics.Collage as Col
import Dict as D
import Maybe as M
import Set as S

-- Basic utility
updateWithDefault : a -> comparable -> (a -> a) -> D.Dict comparable a -> D.Dict comparable a
updateWithDefault def name fn dict = D.insert name (fn <| D.findWithDefault def name dict) dict

incDict d table = let inc d (name, amt) = updateWithDefault 0 name ((+) amt) d
                  in foldr (flip inc) d table

decDict d table = let dec d (name, amt) = updateWithDefault 0 name (\n -> n - amt) d
                  in foldr (flip dec) d table

find0 key dict = D.findWithDefault 0 key dict

-- The Model
type Price = [(String, Int)]
data Upgrade = Income String Int | Capacity String Int
data Prerequisite = Tech String | Res String Int
type Resource = { name:String, reqs:[Prerequisite] }
type Technology = { name:String, cost:Price, upgrade:[Upgrade], reqs:[Prerequisite] }

techTable = D.fromList <| map (\t -> (t.name, t)) technologies
technologies = let tech (name, cost, upgrade, reqs) = 
                       {name=name, cost=cost, upgrade=upgrade, reqs=reqs}
               in map tech
                      [ ("Clay Pit", [("Wood", 30)], [Income "Clay" 1], [])
                      , ("Pottery", [("Wood", 25), ("Clay", 25)], [Capacity "Food" 10], [Tech "Clay Pit"])
                      , ("Lumber Yard", [("Wood", 50), ("Stone", 10)], [Income "Wood" 1, Capacity "Wood" 10], [])
                      , ("Quarry", [("Wood", 20), ("Stone", 50)], [Income "Stone" 1], [Tech "Lumber Yard"])
                      , ("Farm", [("Wood", 30), ("Stone", 30)], [Income "Food" 1], [])
                      , ("Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Ore" 1, Income "Salt" 1], [Tech "Lumber Yard", Tech "Quarry", Tech "Farm"])
                      , ("Gold Mine", [("Wood", 50), ("Stone", 20), ("Food", 30)], [Income "Gold" 1], [Tech "Mine"])
                      , ("Blacksmith", [("Wood", 60), ("Stone", 50)], [], [Tech "Mine"])
                      ]

resTable = D.fromList <| map (\r -> (r.name, r)) resources
resources = let res (name, reqs) = {name=name, reqs=reqs}
            in map res
                   [ ("Wood", []), ("Stone", []),  ("Food", []), ("Clay", []), ("Workers", [Tech "Dwelling"])
                   , ("Ore", [Tech "Mine"]), ("Salt", [Tech "Mine"]), ("Gold", [Tech "Gold Mine"])
                   , ("Money", [Tech "Printing Press", Tech "Mint"]), ("Reputation", [Tech "Printing Press"])
                   , ("Trade", [Tech "Shipyard"]), ("Credit", [Tech "Banking"]), ("Knowledge", [Tech "Philosophy"])
                   ]

type GameState = { skill: D.Dict String Int
                 , income: D.Dict String Int
                 , workers: D.Dict String Int
                 , capacity: D.Dict String Int
                 , built: S.Set String
                 , balance: D.Dict String Int
                 }

defaultGame : GameState
defaultGame = 
    { skill = D.empty
    , income = D.empty
    , workers = D.empty
    , capacity = D.fromList [("Wood", 50), ("Stone", 100), ("Food", 50), ("Salt", 50), ("Ore", 50), ("Gold", 50), ("Clay", 50)]
    , built = S.empty
    , balance = D.fromList [("Workers", 2), ("Food", 10)]
    }

resCap g res = find0 res g.capacity

incBalance g amounts = let inc (res, amt) g = { g | balance <- updateWithDefault 0 res (\n -> min (resCap g res) (n + amt)) g.balance }
                       in foldr inc g amounts

meetsPrereqs : GameState -> [Prerequisite] -> Bool
meetsPrereqs g reqs = let meets prereq = case prereq of
                                           Tech name -> S.member name g.built
                                           Res name amt -> amt < find0 name g.balance
                      in and <| map meets reqs

canAfford : GameState -> Price -> Bool
canAfford g cost = and <| map (\(res, amt) -> amt <= find0 res g.balance) cost

tick : GameState -> GameState
tick g = let new = incBalance g <| D.toList g.income
         in incBalance new <| D.toList g.workers

gather : GameState -> String -> GameState
gather g res = let increment = sum [1, find0 res g.skill, find0 res g.workers]
               in incBalance g [(res, increment)]

build : GameState -> String -> GameState
build g name = case D.lookup name techTable of
                 Nothing -> g
                 Just t -> case and [canAfford g t.cost, meetsPrereqs g t.reqs] of
                             True -> applyUpgrades { g | balance <- decDict g.balance t.cost
                                                   , built <- S.insert t.name g.built
                                                   } t.upgrade
                             _ -> g

assign : GameState -> String -> GameState
assign g name = case 0 < find0 "Workers" g.balance of
                  True  -> { g | workers <- incDict g.workers [(name, 1)]
                               , balance <- decDict g.balance [("Workers", 1)] }
                  False -> g

free : GameState -> String -> GameState
free g name = case 0 < find0 name g.workers of
                True  -> { g | workers <- decDict g.workers [(name, 1)]
                             , balance <- incDict g.balance [("Workers", 1)] }
                False -> g

applyUpgrades : GameState -> [Upgrade] -> GameState
applyUpgrades g ups = foldr (flip applyUpgrade) g ups

applyUpgrade : GameState -> Upgrade -> GameState
applyUpgrade g up = case up of
                      Income res amt -> {g | income <- incDict g.income [(res, amt)]}
                      Capacity res amt -> {g | capacity <- incDict g.capacity [(res, amt)]}

applyEvent : GameState -> Event -> GameState
applyEvent g ev = case ev of
                    TechClick n -> build g n
                    ResClick n -> gather g n
                    Assign n -> assign g n
                    Free n -> free g n
                    Tick -> tick g

-- The Inputs
data Event = TechClick String | ResClick String | Assign String | Free String | Tick
techGroup = In.buttons <| TechClick ""
resourceGroup = In.buttons <| ResClick ""
assignGroup = In.buttons <| Assign ""
freeGroup = In.buttons <| Free ""

clock = sampleOn (every second) <| constant Tick

events = merges [techGroup.events, resourceGroup.events, assignGroup.events, freeGroup.events, clock]

-- The Game
game = foldp (\val memo -> applyEvent memo val) defaultGame events

-- The View
spc = spacer 10 10
lspaced elem = beside spc elem
rspaced elem = beside elem spc

techTemplate t = let (w, h) = (150, 180)
                 in rspaced <| layers [ collage w h [ filled gray <| Col.rect w h ]
                                      , flow down [ height 30 <| width w <| techGroup.button (TechClick t.name) t.name, spc 
                                                  , beside spc <| (flow down [ plainText "Cost:" `above` (lspaced (flow down <| map asText t.cost))
                                                                             , plainText "Upgrade:" `above` (lspaced (flow down <| map asText t.upgrade))])]]

resourceTemplate : Resource -> Int -> Element
resourceTemplate r assigned = 
    let (w, h) = (81, 55)
    in rspaced <| flow down [ height 30 . width w <| resourceGroup.button (ResClick r.name) r.name
                            , flow right [ height 25 . width (round <| w / 3) <| assignGroup.button (Free r.name) "-"
                                         , spc, asText assigned, spc
                                         , height 25 . width (round <| w / 3) <| assignGroup.button (Assign r.name) "+"]]

techButtons g = let relevants = filter (meetsPrereqs g . .reqs) technologies
                in map techTemplate relevants

resourceButtons g = let relevants = filter (meetsPrereqs g . .reqs) resources 
                        btn r = resourceTemplate r <| find0 r.name g.workers
                    in map btn relevants

showGame : GameState -> Element
showGame g = flow down [ flow right <| resourceButtons g
                       , asText <| D.toList g.balance
                       , asText <| D.toList g.workers
                       , asText <| D.toList g.income
                       , asText <| D.toList g.capacity
                       , asText <| S.toList g.built
                       , flow right <| techButtons g ]

main = lift showGame game

-- Basic Logging
printEvent ev = case ev of
                  TechClick n -> "UPGRADE " ++ n
                  ResClick n -> "GATHERED " ++ n
                  Assign n -> "ASSIGNING TO " ++ n
                  Free n -> "FREEING FROM " ++ n
                  Tick -> "TICK"

port log : Signal String
port log = lift printEvent events
