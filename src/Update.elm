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
      , frustration = max (kid.frustration - usefulDelta * gameConstants.frustrationRecovery) 0
      , mutedCooldown = max (kid.mutedCooldown - deltaSeconds) 0
      , kidDialogCooldown = max (kid.kidDialogCooldown - deltaSeconds) 0
      , playerDialogCooldown = max (kid.playerDialogCooldown - deltaSeconds) 0
    }


nervesGrowthPerKid : Kid -> Float
nervesGrowthPerKid kid =
  if not (isKidIncreasingNerves kid) then 0
  else
    let
      threshold = gameConstants.nervesActivityGrowthThreshold
    in
      ((kid.activity - threshold) / (1 - threshold)) * gameConstants.nervesActivityGrowth



updateNerves : Float -> Model -> Float --return new nerves
updateNerves deltaSeconds model =
  let
    deltaPerSecond =
      case model.playerActivity of
        DeepBreath -> -gameConstants.deepBreathNervesRecovery
        CalmDownKid _ -> (gameConstants.calmDownNervesGrowth / gameConstants.calmDownDuration)
        _ ->       
          gameConstants.nervesBaseGrowth
          + ( List.map nervesGrowthPerKid model.kids
              |> List.sum )

  in
    defaultClamp ( model.nerves + deltaPerSecond * deltaSeconds )

updateActivity : Float -> Model -> Model
updateActivity deltaSeconds model =
    case model.playerActivity of
      CalmDownKid calmDownInfo ->
        let 
          newCalmDownDuration = calmDownInfo.duration + deltaSeconds 
        in
          if newCalmDownDuration >= gameConstants.calmDownDuration then
            performKidCalmdown calmDownInfo { model | playerActivity = None}
          else
            {model | 
              playerActivity = CalmDownKid {calmDownInfo | duration = newCalmDownDuration}
            } 
      _ -> model --ignoring deepBreath here as it is handled in updateNerves 


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
        |> updateActivity deltaSeconds
    in
      (
        {model |
          nerves = updateNerves deltaSeconds model
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
          if model.timeToOutburst <= 0 then 
            Random.generate (gameMsg PerformOutburst) 
              (Random.map2 --combine the two generators to create an appropriate pair 
                (\target intensity -> {target = target, intensity = intensity}) 
                (RandomGenerators.outburstTarget model.kids) 
                (RandomGenerators.outburstIntensity)
              )
          else Cmd.none
        )
      )

updateKidById : Int -> (Kid -> Kid) -> List Kid -> List Kid
updateKidById kidId updateFunction kids =
  let 
    mapFunction = \kid -> if kid.id == kidId then updateFunction kid else kid
  in
    List.map mapFunction kids

kidCalmDownFunction : Float -> Kid -> Kid
kidCalmDownFunction nerves kid =
  let
    effectivity = if 1 - nerves >= gameConstants.calmDownNervesGrowth then 1
                    else  (1 - nerves) / gameConstants.calmDownNervesGrowth
  in
    {kid |
      activity = (effectivity * kid.activity * gameConstants.calmDownActivityMultiplier)
                  + ( (1.0 - effectivity) * kid.activity)
      , frustration =
          defaultClamp (
            kid.frustration
              + gameConstants.calmDownFrustrationGrowthMin
              + (
                (gameConstants.calmDownFrustrationGrowthMax - gameConstants.calmDownFrustrationGrowthMin) 
                * (nerves ^ gameConstants.calmDownFrustrationGrowthExponent)
              )
          )
      , mutedCooldown = gameConstants.calmDownMutedTime
      , shownPlayerDialog = Texts.getDialogString (Texts.calmDownDialog nerves)
      , playerDialogCooldown = gameConstants.dialogCooldown
    }

performKidCalmdown : CalmDownInfo -> Model -> Model
performKidCalmdown calmDownInfo model =
  {model |
    kids = updateKidById calmDownInfo.kidId (kidCalmDownFunction calmDownInfo.nervesAtStart) model.kids
  }

performKidOutburst : OutburstParams -> Kid -> Kid
performKidOutburst params kid =
  let    
    minGrowth = gameConstants.outburstMinActivityGrowth
    maxGrowth = gameConstants.outburstMaxActivityGrowth
    intensity = kid.waywardness * params.intensity --the intensity should probably be randomized
  in
    {kid |
      activity = defaultClamp kid.activity + minGrowth + (intensity * (maxGrowth - minGrowth))
      , shownKidDialog = Texts.getDialogString (Texts.outburstDialog intensity)
      , kidDialogCooldown = gameConstants.dialogCooldown
    }

performOutburst : OutburstParams -> Model -> Model
performOutburst params model =
  {model |
    kids = RandomGenerators.outburstTargetFilter params.target (performKidOutburst params) model.kids
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
        { model | playerActivity = DeepBreath }
        ! []
      DeepBreathEnded ->
        { model | playerActivity = None}
        ! []
      CalmDownStarted kid ->
        { model | playerActivity = CalmDownKid { duration = 0, kidId = kid.id, nervesAtStart = model.nerves } }
        ! []
      CalmDownEnded ->
        { model | playerActivity = None}
        ! []      
      ScheduleOutburst delta ->
        {model | timeToOutburst = delta}
        ! []
      PerformOutburst params ->
        (
          performOutburst params model
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
