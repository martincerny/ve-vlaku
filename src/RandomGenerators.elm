module RandomGenerators exposing (
  outburstParams
  )

import GameConstants exposing(..)
import Random
import Model exposing(..)

exponentialInverseCDF : Float -> Float -> Float
exponentialInverseCDF mean y =
  -mean * logBase e (1 - y) 


outburstInterval : Float -> Random.Generator Float
outburstInterval waywardness = 
  let
    totalMinInterval = 
      gameConstants.minOutburstInterval
    minMeanInterval = 
      gameConstants.meanOutburstIntervalMin
    maxMeanInterval = 
      gameConstants.meanOutburstIntervalMax
    actualMeanInterval =
      (minMeanInterval + (1 - waywardness) * (maxMeanInterval - minMeanInterval)) 
      - totalMinInterval
  in
    Random.map (exponentialInverseCDF actualMeanInterval >> (+) 2) (Random.float 0 1)

outburstIntensity : Random.Generator Float
outburstIntensity =
  Random.float 0 1

outburstParams : Int -> Float -> Random.Generator OutburstParams
outburstParams kidId waywardness =
  Random.map2 
    (\interval intensity -> {targetKidId = kidId, interval = interval, intensity = intensity})
    (outburstInterval waywardness)
    outburstIntensity