module Model exposing (Kid, Model, defaultKid, nervesGrowth, isHighActivity, isKidHighActivity, isKidIncreasingNerves)

import GameConstants exposing (..)
import Utils

type alias Kid = 
  { 
    id : Int
    , name : String
    , waywardness : Float
    , activity: Float
    , activityGrowthCooldown: Float   
  }

type alias Model = 
  { nerves : Float  
    , kids : List Kid
    , takingDeepBreath : Bool
    , lost : Bool
    , highActivityTime : Float
  }

-- Constructors

defaultKid : Kid
defaultKid =
  { id = -1
  , name = ""
  , waywardness = 0
  , activity = 0
  , activityGrowthCooldown = 0 
  }

-- Computed properties

nervesGrowth : Model -> Float
nervesGrowth model =
  let
    averageActivity = 
      Utils.avg (List.map .activity model.kids)
  in
    gameConstants.nervesBaseGrowth  
    + if averageActivity > gameConstants.nervesActivityGrowthThreshold  
        then gameConstants.nervesActivityGrowth  * ((averageActivity - gameConstants.nervesActivityGrowthThreshold) / (1 - gameConstants.nervesActivityGrowthThreshold))
        else 0

isKidHighActivity : Kid -> Bool
isKidHighActivity kid =
  kid.activity > gameConstants.highActivityThreshold

isHighActivity : Model -> Bool
isHighActivity model =
  let
    numActiveKids = 
      List.length (List.filter isKidHighActivity model.kids)
    in 
      numActiveKids >= gameConstants.highActivityKidsToFail

isKidIncreasingNerves : Kid -> Bool
isKidIncreasingNerves kid =
  kid.activity > gameConstants.nervesActivityGrowthThreshold