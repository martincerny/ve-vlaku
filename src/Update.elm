module Update exposing (update)

import Time
import Msg exposing (Msg(..))

import Model exposing (..)
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

isHighActivity : Model -> Bool
isHighActivity model =
  let
    numActiveKids = 
      List.length (List.filter (\kid -> kid.activity > gameConstants.highActivityThreshold) model.kids)
    in 
      numActiveKids >= gameConstants.highActivityKidsToFail

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Frame delta ->
      let 
        deltaSeconds = 
          delta / Time.second
      in
      (
        if model.lost then model
        else
          {model | 
            nerves = defaultClamp ( model.nerves +
              if model.takingDeepBreath then 
                  -gameConstants.deepBreathNervesRecovery * deltaSeconds
              else
                  (nervesGrowth model) * deltaSeconds 
            ) 
            , highActivityTime =
              if isHighActivity model then model.highActivityTime + deltaSeconds
              else 0   
            , lost =
              if model.highActivityTime >= gameConstants.highActivityTimeToFail then True
              else model.lost
            , kids = List.map (updateKid deltaSeconds) model.kids
          }
        , Cmd.none        
      )
    DeepBreathStarted ->
        ({ model | takingDeepBreath = True}, Cmd.none)
    DeepBreathEnded ->
        ({ model | takingDeepBreath = False}, Cmd.none)
