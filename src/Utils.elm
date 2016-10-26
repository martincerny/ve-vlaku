module Utils exposing (avg, fixedWidthNumberFormat)

import String


avg : List Float -> Float
avg list =
    (List.sum list) / (toFloat (List.length list))


fixedWidthNumberFormat : Int -> Int -> String
fixedWidthNumberFormat width number =
    String.padLeft width '0' (toString number)
