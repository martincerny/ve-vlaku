module Update exposing (update)

import Time
import Msg exposing (..)

import Model exposing (..)
import GameConstants exposing(gameConstants)
import Init exposing(init)
import Texts
import Random 
import RandomGenerators

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
      activity = 
        defaultClamp ( kid.activity + usefulDelta * kid.waywardness * 
          (gameConstants.activityBaseGrowth + gameConstants.activityFrustrationGrowth * kid.frustration) 
        )
      , mutedCooldown = max (kid.mutedCooldown - deltaSeconds) 0   
      , kidDialogCooldown = max (kid.kidDialogCooldown - deltaSeconds) 0   
      , playerDialogCooldown = max (kid.playerDialogCooldown - deltaSeconds) 0   
    }

updateGameFrame : Float -> Model -> (Model, Cmd Msg)
updateGameFrame deltaSeconds oldModel =
    let 
      model = 
        setState oldModel (
          if oldModel.highActivityScore >= gameConstants.highActivityScoreToLose then Lost Activity
          else if oldModel.nerves >= 1 then Lost Nerves 
          else if not (isStateLost oldModel.state) && oldModel.timeToWin <= 0 then Won
          else oldModel.state
        )
    in
      (
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
          , timeToWin = max 0 (model.timeToWin - deltaSeconds)
          , timeToOutburst = max 0 (model.timeToOutburst - deltaSeconds)
          , kids = List.map (updateKid deltaSeconds) model.kids
        } 
      ,
        (
          if model.timeToOutburst <= 0 then Random.generate (gameMsg PerformOutburst) (RandomGenerators.outburstTarget model.kids)
          else Cmd.none
        )
      )

kidCalmDownMapFunction : Int -> Float -> Kid -> Kid
kidCalmDownMapFunction kidId nerves kid =
  let 
    effectivity = if 1 - nerves >= gameConstants.calmDownNervesGrowth then 1
                    else  (1 - nerves) / gameConstants.calmDownNervesGrowth
  in
    if kid.id == kidId then 
      {kid |
        activity = (effectivity * kid.activity * gameConstants.calmDownActivityMultiplier)
                    + ( (1.0 - effectivity) * kid.activity)
        , frustration = 
            defaultClamp (
              kid.frustration 
                + gameConstants.frustrationGrowthFromNerves * (nerves ^ gameConstants.frustrationGrowthFromNervesExponent) 
            )
        , mutedCooldown = gameConstants.calmDownMutedTime
        , shownPlayerDialog = Texts.getDialogString (Texts.calmDownDialog nerves)
        , playerDialogCooldown = gameConstants.dialogCooldown                  
      } 
    else kid

performKidCalmdown : Int -> Model -> Model
performKidCalmdown kidId model =
  if List.any (\kid -> kid.id == kidId && not (isMuted kid)) model.kids then --check if the target kid is not muted
    {model |
      nerves = defaultClamp (model.nerves + gameConstants.calmDownNervesGrowth)
      , kids = List.map (kidCalmDownMapFunction kidId model.nerves) model.kids 
    }
  else model

performKidOutburst : Kid -> Kid 
performKidOutburst kid =
  let
    intensity = kid.waywardness --the intensity should probably be randomized
  in
    {kid |
      activity = defaultClamp kid.activity + (intensity * gameConstants.outburstActivityGrowth)
      , shownKidDialog = Texts.getDialogString (Texts.outburstDialog intensity)
      , kidDialogCooldown = gameConstants.dialogCooldown
    }

performOutburst : Float -> Model -> Model
performOutburst randomValue model =
  {model |
    kids = RandomGenerators.outburstTargetFilter randomValue performKidOutburst model.kids
  } 

updateUIFrame : Float -> Model -> Model
updateUIFrame deltaSeconds model =
  if model.transitionInactivity > 0 then 
    {model | transitionInactivity = max 0 (model.transitionInactivity - deltaSeconds)}
  else
    model
  

processUIMessage : UIMessage -> Model -> (Model, Cmd Msg)
processUIMessage msg model =
  if model.transitionInactivity > 0 then model ! []
  else
  (
  case msg of 
    ResumeGame -> 
      (setState model Running) ! []
    RestartGame -> 
      init
  )

processGameMessage : GameMessage -> Model -> (Model, Cmd Msg)
processGameMessage msg model =
  if not (shouldUpdateGame model) then (model, Cmd.none)
  else
    case msg of
      DeepBreathStarted ->
        { model | takingDeepBreath = True} 
        ! []
      DeepBreathEnded ->
        { model | takingDeepBreath = False} 
        ! []
      CalmDown kid ->
        performKidCalmdown kid.id model 
        ! []
      ScheduleOutburst delta ->  
        {model | timeToOutburst = delta}
        ! []
      PerformOutburst randomValue ->
        (
          performOutburst randomValue model
          ,Random.generate (gameMsg ScheduleOutburst) RandomGenerators.outburstSchedule
        )



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  case msg of
    UI uiMsg ->
      processUIMessage uiMsg model
    Game gameMsg -> 
      processGameMessage gameMsg model
    Frame delta ->
      let
        deltaSeconds = 
          delta / Time.second
      in
        if shouldUpdateGame model then 
          updateGameFrame deltaSeconds model
        else
          (updateUIFrame deltaSeconds model) ! []
