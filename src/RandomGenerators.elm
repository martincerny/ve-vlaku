module RandomGenerators exposing (outburstSchedule, outburstTarget, outburstTargetFilter)

import GameConstants exposing(..)
import Random
import Model exposing(..)

outburstSchedule : Random.Generator Float
outburstSchedule = 
  Random.float gameConstants.minOutburstInterval gameConstants.maxOutburstInterval

kidToOutburstProbability : Kid -> Float 
kidToOutburstProbability kid =
  if isMuted kid then 0
  else kid.waywardness

outburstTarget : List Kid -> Random.Generator Float
outburstTarget kids = 
  Random.float 0 (
    List.map kidToOutburstProbability kids 
      |> List.sum
  )

outburstTargetFilterMapper : (Kid -> Kid) -> List Kid -> Float -> Float -> List Kid -> List Kid
outburstTargetFilterMapper mapFunction kidsToProcess value waywardnessSum processedKids =
  case kidsToProcess of
    [] -> 
      List.reverse processedKids
    kid :: remainingKids ->
      let 
        newWaywardnessSum = 
          waywardnessSum + kidToOutburstProbability kid
        processedCurrentKid = 
          if value < newWaywardnessSum then mapFunction kid
          else kid
      in 
        outburstTargetFilterMapper mapFunction remainingKids value newWaywardnessSum (processedCurrentKid :: processedKids)



outburstTargetFilter : Float -> (Kid -> Kid) -> List Kid -> List Kid 
outburstTargetFilter value mapFunction kids = 
  outburstTargetFilterMapper mapFunction kids value 0 []
