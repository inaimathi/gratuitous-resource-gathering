module Grg where

import Graphics.Input as In
import Graphics.Collage as Col
import Dict as D

import open Grg.Util
import open Grg.Data
import open Grg.Model

-- The Inputs
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

-- -- Basic Logging
-- printEvent ev = case ev of
--                   TechClick n -> "UPGRADE " ++ n
--                   ResClick n -> "GATHERED " ++ n
--                   Assign n -> "ASSIGNING TO " ++ n
--                   Free n -> "FREEING FROM " ++ n
--                   Tick -> "TICK"
--                   _ -> "The fuck? " ++ (show ev)

-- port log : Signal String
-- port log = lift printEvent events
