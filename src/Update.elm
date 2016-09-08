module Update exposing (update)

import Time
import Msg exposing (Msg(..))

import Model exposing (..)
import Utils 
import GameConstants exposing(gameConstants)

defaultClamp : Float -> Float
defaultClamp = 
  clamp 0 1

updateKid : Float -> Kid -> Kid
updateKid deltaSeconds kid =
  let usefulDelta = 
    if deltaSeconds < kid.activityGrowthCooldown then 0
    else deltaSeconds - kid.activityGrowthCooldown
  in
    {kid |
      activity = defaultClamp ( kid.activity + usefulDelta * kid.waywardness * (gameConstants.activityBaseGrowth) )
      , activityGrowthCooldown = max (kid.activityGrowthCooldown - deltaSeconds) 0   
    }

computeNervesGrowth : Model -> Float
computeNervesGrowth model =
  let
    averageActivity = 
      Utils.avg (List.map .activity model.kids)
  in
    gameConstants.nervesBaseGrowth  
    + if averageActivity > gameConstants.nervesActivityGrowthThreshold  
        then gameConstants.nervesActivityGrowth  * ((averageActivity - gameConstants.nervesActivityGrowthThreshold) / (1 - gameConstants.nervesActivityGrowthThreshold))
        else 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Frame delta ->
      let 
        deltaSeconds = 
          delta / Time.second
      in
      (
        {model | 
          nerves = defaultClamp ( model.nerves + (computeNervesGrowth model) * deltaSeconds )
          , kids = List.map (updateKid deltaSeconds) model.kids
        }
        , Cmd.none        
      )
