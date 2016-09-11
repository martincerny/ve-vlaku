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

updateFrame : Float -> Model -> Model
updateFrame delta model =
    let 
      deltaSeconds = 
        delta / Time.second
    in
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

kidCalmDownMapFunction : Int -> Float -> Kid -> Kid
kidCalmDownMapFunction kidId effectivity kid =
  if kid.id == kidId then 
    {kid |
      activity = (effectivity * kid.activity * gameConstants.calmDownActivityMultiplier)
                  + ( (1.0 - effectivity) * kid.activity) 
      , activityGrowthCooldown = gameConstants.activityGrowthCooldown                  
    } 
  else kid

performKidCalmdown : Int -> Model -> Model
performKidCalmdown kidId model =
  let 
    effectivity = if 1 - model.nerves >= gameConstants.calmDownNervesGrowth then 1
                    else  (1 - model.nerves) / gameConstants.calmDownNervesGrowth
    kidsMapFunction = effectivity                    
  in
    {model |
      nerves = defaultClamp (model.nerves + gameConstants.calmDownNervesGrowth)
      , kids = List.map (kidCalmDownMapFunction kidId effectivity) model.kids 
    }



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  if model.lost then (model, Cmd.none)
  else
    case msg of
      Frame delta ->
        (
          updateFrame delta model
          , Cmd.none        
        )
      DeepBreathStarted ->
        ({ model | takingDeepBreath = True}, Cmd.none)
      DeepBreathEnded ->
        ({ model | takingDeepBreath = False}, Cmd.none)
      CalmDown kid ->
        (performKidCalmdown kid.id model, Cmd.none)
