module UpdateUtils exposing (defaultClamp, followTargetValue)


defaultClamp : Float -> Float
defaultClamp =
    clamp 0 1


followTargetValue : Float -> Float -> Float -> Float -> Float
followTargetValue target halfTime deltaSeconds currentValue =
    let 
        diff = target - currentValue
        change = (diff / halfTime) * deltaSeconds
    in 
        if abs change >= abs diff then
            target
        else 
            currentValue + change  
