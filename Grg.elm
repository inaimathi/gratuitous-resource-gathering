module Grg where

import Graphics.Input as In
import Graphics.Collage as Col
import open Maybe
import Dict as D

-- Basic utility
updateWithDefault : a -> comparable -> (a -> a) -> D.Dict comparable a -> D.Dict comparable a
updateWithDefault def name fn dict = D.insert name (fn <| D.findWithDefault def name dict) dict

incDict d table = let inc (name, amt) d = updateWithDefault 0 name ((+) amt) d
                  in foldr inc d table

decDict d table = let dec (name, amt) d = updateWithDefault 0 name (\n -> n - amt) d
                  in foldr dec d table

find0 key dict = D.findWithDefault 0 key dict

-- The Model
data Reps = Count Int | One | Inf
type Price = [(String, Int)]
data Upgrade = Income String Int | Capacity String Int
data Prerequisite = Tech String | Res String Int
type Resource = { name:String, reqs:[Prerequisite], upkeep:[(String, Float)] }
type Technology = { name:String, cost:Price, upgrade:[Upgrade], reqs:[Prerequisite], repeatable:Reps }

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

type GameState = { skill: D.Dict String Int
                 , income: D.Dict String Int
                 , workers: D.Dict String Int
                 , capacity: D.Dict String Int
                 , built: D.Dict String Int
                 , balance: D.Dict String Int }

defaultGame : GameState
defaultGame = 
    applyUpkeep { skill = D.empty, income = D.empty, built = D.empty
                , workers = D.fromList [("Food", 3)], balance = D.fromList [("Food", 24), ("Workers", 0)]
                , capacity = D.fromList [("Workers", 5), ("Wood", 50), ("Stone", 100), ("Food", 50), ("Salt", 50), ("Ore", 50), ("Gold", 50), ("Clay", 50)]
                }

resCap : GameState -> String -> Int
resCap g res = find0 res g.capacity

resTotal : GameState -> String -> Int
resTotal g res = let balance = find0 res g.balance
                 in case res of 
                      "Workers" -> balance + (sum <| D.values g.workers)
                      _         -> balance

techBuilt : GameState -> String -> Int
techBuilt g tech = find0 tech g.built

incBalance : GameState -> [(String, Int)] -> GameState
incBalance g amounts = let inc (res, amt) g = { g | balance <- updateWithDefault 0 res (\n -> min (resCap g res) (n + amt)) g.balance }
                       in foldr inc g amounts

meetsPrereqs : GameState -> [Prerequisite] -> Bool
meetsPrereqs g reqs = let meets prereq = case prereq of
                                           Tech name -> (0 < techBuilt g name)
                                           Res name amt -> amt < resTotal g name
                      in and <| map meets reqs

haventMaxed : GameState -> Technology -> Bool
haventMaxed g tech = let built = techBuilt g tech.name
                     in case tech.repeatable of
                          Inf -> True
                          One -> 1 > built
                          Count num -> num > built

canAfford : GameState -> Price -> Bool
canAfford g cost = and <| map (\(res, amt) -> amt <= find0 res g.balance) cost

decResource : GameState -> (String, Int) -> GameState
decResource g (name, delta) = 
    case name of
      "Workers" -> if canAfford g [(name, delta)] 
                   then { g | balance <- decDict g.balance [(name, delta)] }
                   else decResource { g | balance <- incDict g.balance [("Workers", sum <| D.values g.workers)]
                                        , workers <- D.empty } (name, delta)
      _ -> { g | balance <- decDict g.balance [(name, delta)] }

upkeepOf : GameState -> (String, Int) -> Price
upkeepOf g (name, _) = let raw = maybe [] .upkeep <| D.lookup name resTable
                           balance = resTotal g name
                           price = map (\(name, u) -> (name, round <| u * (toFloat <| balance))) raw
                           penalty = case price of
                                       [] -> []
                                       lst -> [(name, 1)]
                       in if canAfford g price then price else penalty

applyUpkeep : GameState -> GameState
applyUpkeep g = let upk res g = foldr (flip decResource) g <| upkeepOf g res
                in foldr upk g <| D.toList g.balance

tick : GameState -> GameState
tick g = let paidOff = applyUpkeep g
             new = incBalance paidOff <| D.toList paidOff.income
         in incBalance new <| D.toList paidOff.workers

gather : GameState -> String -> GameState
gather g res = let increment = sum [1, find0 res g.skill, find0 res g.workers]
               in incBalance g [(res, increment)]

build : GameState -> String -> GameState
build g name = case D.lookup name techTable of
                 Nothing -> g
                 Just t -> case and [canAfford g t.cost, meetsPrereqs g t.reqs, haventMaxed g t] of
                             True  -> applyUpgrades { g | balance <- decDict g.balance t.cost
                                                        , built <- incDict g.built [(t.name, 1)]
                                                    } t.upgrade
                             False -> g
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

techButtons g = let relevants = filter (\t -> and [meetsPrereqs g t.reqs, haventMaxed g t]) technologies
                in map techTemplate relevants

resourceButtons g = let relevants = filter (meetsPrereqs g . .reqs) resources 
                        btn r = resourceTemplate r <| find0 r.name g.workers
                    in map btn relevants

showGame : GameState -> Element
showGame g = flow down [ flow right <| resourceButtons g
                       , asText <| D.toList g.balance
                       , asText <| D.toList g.workers
                       , asText <| map (upkeepOf g) <| D.toList g.balance
                       , asText <| D.toList g.income
                       , asText <| D.toList g.built
                       , flow right <| techButtons g ]

main = lift showGame game

-- Basic Logging
printEvent ev = case ev of
                  TechClick n -> "UPGRADE " ++ n
                  ResClick n -> "GATHERED " ++ n
                  Assign n -> "ASSIGNING TO " ++ n
                  Free n -> "FREEING FROM " ++ n
                  Tick -> "TICK"
                  _ -> "The fuck? " ++ (show ev)

port log : Signal String
port log = lift printEvent events
