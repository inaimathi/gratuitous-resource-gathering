module Test where

import Mouse
import Time

-- port delta : Signal Int

port wood : Signal Bool
port building : Signal String

buildingCost = foldp (+) 0 <| keepWhen (lift2 (>) (constant 50) balance) 0 <| sampleOn building <| constant 50

tickIncrement = foldp (+) 0 <| sampleOn buildingCost <| constant 0.01
tick = sampleOn (every Time.millisecond) <| constant True

spent = foldp (+) 0 <| merges [ sampleOn building buildingCost ]

gathered = foldp (+) 0 <| merges [ sampleOn wood <| constant 1
                                 , sampleOn tick tickIncrement ]

balance = lift round <| lift2 (-) gathered spent

main = lift (flow down) <| combine [ lift asText balance ]
