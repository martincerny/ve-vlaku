module UpdateUtils exposing (defaultClamp)


defaultClamp : Float -> Float
defaultClamp =
    clamp 0 1
