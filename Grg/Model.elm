module Grg.Model where

import Dict as D
import open Maybe

import open Grg.Data
import open Grg.Util

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
