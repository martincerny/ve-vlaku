module Utils exposing (avg)

avg : List Float -> Float
avg list =
  (List.sum list) / (toFloat (List.length list))
