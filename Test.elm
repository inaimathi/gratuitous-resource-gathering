module Test where

import Mouse
import Time

port gather : Signal Bool
port build : Signal String

costIncrement = constant 50
cost = foldp (+) 0 <| keepWhen canAfford 0 <| sampleOn build costIncrement
nextCost = lift2 (+) cost costIncrement

spent = foldp (+) 0 <| merges [ sampleOn build cost ]

gathered = foldp (+) 0 <| merges [ sampleOn gather <| constant 1, sampleOn tick tickIncrement ]

balance = lift round <| lift2 (-) gathered spent

canAfford = lift2 (>=) balance <| lift round nextCost

tickIncrement = foldp (+) 0 <| sampleOn cost <| constant 0.01
tick = sampleOn (every Time.millisecond) <| constant True

main = lift (flow down) <| combine [ lift asText balance, lift asText canAfford, lift asText spent, lift asText gathered, lift asText nextCost ]
