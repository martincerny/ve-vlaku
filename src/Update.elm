module Update exposing (update)

import Time
import Msg exposing (Msg(..))

import Model exposing (..)
import GameConstants exposing(gameConstants)
import Init exposing(init)

defaultClamp : Float -> Float
defaultClamp = 
  clamp 0 1

updateKid : Float -> Kid -> Kid
updateKid deltaSeconds kid =
  let usefulDelta = 
    if deltaSeconds < kid.mutedCooldown then 0
    else deltaSeconds - kid.mutedCooldown
  in
    {kid |
      activity = defaultClamp ( kid.activity + usefulDelta * kid.waywardness * (gameConstants.activityBaseGrowth) )
      , mutedCooldown = max (kid.mutedCooldown - deltaSeconds) 0   
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
        , highActivityScore =
            let 
              numHighActivityKids = List.filter isKidHighActivity model.kids |> List.length
            in
              if numHighActivityKids > 0 then 
                model.highActivityScore + deltaSeconds * (toFloat numHighActivityKids) * gameConstants.highActivityScoreIncreasePerKid
              else 
                max 0 (model.highActivityScore - deltaSeconds * gameConstants.highActivityScoreRecovery)   
        , lost =
          if model.highActivityScore >= gameConstants.highActivityScoreToLose then True
          else model.lost
        , kids = List.map (updateKid deltaSeconds) model.kids
      }

kidCalmDownMapFunction : Int -> Float -> Kid -> Kid
kidCalmDownMapFunction kidId effectivity kid =
  if kid.id == kidId then 
    {kid |
      activity = (effectivity * kid.activity * gameConstants.calmDownActivityMultiplier)
                  + ( (1.0 - effectivity) * kid.activity) 
      , mutedCooldown = gameConstants.calmDownMutedTime                  
    } 
  else kid

performKidCalmdown : Int -> Model -> Model
performKidCalmdown kidId model =
  if List.any (\kid -> kid.id == kidId && not (isMuted kid)) model.kids then --check if the target kid is not muted
    let 
      effectivity = if 1 - model.nerves >= gameConstants.calmDownNervesGrowth then 1
                      else  (1 - model.nerves) / gameConstants.calmDownNervesGrowth
      kidsMapFunction = effectivity                    
    in
      {model |
        nerves = defaultClamp (model.nerves + gameConstants.calmDownNervesGrowth)
        , kids = List.map (kidCalmDownMapFunction kidId effectivity) model.kids 
      }
  else model



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    ResumeGame -> 
      ({model | paused = False}, Cmd.none)
    RestartGame -> 
      init

    _ ->
      if model.lost || model.paused then (model, Cmd.none)
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
          ResumeGame ->
            (model, Cmd.none)
          RestartGame ->
            (model, Cmd.none)
