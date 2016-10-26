module Update exposing (update)

import Time
import Msg exposing (..)

import Model exposing (..)
import GameConstants exposing(gameConstants)
import Init exposing(init)
import Emojis
import Random
import RandomGenerators
import Debug

defaultClamp : Float -> Float
defaultClamp =
  clamp 0 1

updateKidDialogs : Float -> Kid -> Kid
updateKidDialogs deltaSeconds kid =
      {kid |
          kidDialogCooldown = max (kid.kidDialogCooldown - deltaSeconds) 0
          , playerDialogCooldown = max (kid.playerDialogCooldown - deltaSeconds) 0
      }


scheduleOutburstCmd : Kid -> Cmd Msg
scheduleOutburstCmd kid =
  Random.generate (gameMsg ScheduleOutburst) (RandomGenerators.outburstParams kid.id kid.waywardness)

updateKidOutburst : Kid -> (Kid, Maybe (Cmd Msg))
updateKidOutburst kid =
  let 
    outburstActive = isActiveOutburst kid.scheduledOutburst
  in 
    if kid.timeSinceLastOutburst < kid.scheduledOutburst.interval && outburstActive then
      (kid, Nothing)
    else
      let    
        minGrowth = gameConstants.outburstMinActivityGrowth
        maxGrowth = gameConstants.outburstMaxActivityGrowth
        intensity = kid.waywardness * kid.scheduledOutburst.intensity 
      in        
        (
          if not outburstActive then kid
          else
            {kid |
              activity = defaultClamp (kid.activity + minGrowth + (intensity * (maxGrowth - minGrowth)))
              , shownKidDialog = Emojis.outburst intensity
              , kidDialogCooldown = gameConstants.dialogCooldown
              , timeSinceLastOutburst = 0
              , scheduledOutburst = emptyOutburstParams
            }
          , Just (scheduleOutburstCmd kid)
        )

updateKidDefault : Float -> Kid -> (Kid, Maybe (Cmd Msg))
updateKidDefault deltaSeconds kid =
  let 
    usefulDelta =
      if deltaSeconds < kid.mutedCooldown then 0
      else deltaSeconds - kid.mutedCooldown
    frustrationThreshold =
      gameConstants.activityFrustrationGrowthThreshold
  in
    {kid |
      activity =
        defaultClamp ( kid.activity + usefulDelta * kid.waywardness *
          (gameConstants.activityBaseGrowth +
            if kid.frustration > frustrationThreshold then               
              gameConstants.activityFrustrationGrowth * 
                ((kid.frustration - frustrationThreshold) / (1 - frustrationThreshold))
            else
              0
          )
        )
      , frustration = max (kid.frustration - usefulDelta * gameConstants.frustrationRecovery) 0
      , timeSinceLastOutburst = kid.timeSinceLastOutburst + usefulDelta --outbursts are not ticking when the kid is muted, so they use usefulDelta
        -- The following use deltaSeconds because those timers should work even when the kid is muted
      , mutedCooldown = max (kid.mutedCooldown - deltaSeconds) 0
    }
    |> (updateKidDialogs deltaSeconds)
    |> updateKidOutburst

calmDownFrustrationRecovery : Float -> Float -> Float
calmDownFrustrationRecovery calmDownDuration oldFrustration =
  if calmDownDuration <= gameConstants.calmDownFrustrationRecoveryStart then
    0
  else 
    gameConstants.calmDownDurationFrustrationRecovery

kidCalmDownActivityRecovery : Kid -> Float
kidCalmDownActivityRecovery kid =
  (kid.activity / (2 * gameConstants.calmDownActivityRecoveryHalfTime))

updateKidCalmDown : CalmDownInfo -> Float -> Kid -> (Kid, Maybe (Cmd Msg))
updateKidCalmDown calmDownInfo deltaSeconds kid =
    ({kid |
      activity = defaultClamp (kid.activity - deltaSeconds * (kidCalmDownActivityRecovery kid) )
      , frustration = defaultClamp (kid.frustration - deltaSeconds * (calmDownFrustrationRecovery calmDownInfo.duration kid.frustration))
      , mutedCooldown = gameConstants.calmDownMutedTime --always reset the cooldown 
    }
    |> (updateKidDialogs deltaSeconds)
    , Nothing)

updateSingleKid : PlayerActivity -> Float -> Kid -> (Kid, Maybe (Cmd Msg))
updateSingleKid playerActivity deltaSeconds kid =  
    case playerActivity of
      CalmDownKid calmDownInfo ->
        if calmDownInfo.kidId == kid.id then
          updateKidCalmDown calmDownInfo deltaSeconds kid
        else
          updateKidDefault deltaSeconds kid
      _ ->
        updateKidDefault deltaSeconds kid

addPairWithMaybeToListOfPairs : (a, Maybe b) -> (List a, List b) -> (List a, List b)
addPairWithMaybeToListOfPairs (x, maybeY) (listOfXs, listOfYs) =
  let 
    newListOfYs =
      case maybeY of 
        Just y ->
          y :: listOfYs
        Nothing ->
          listOfYs
  in 
    (x :: listOfXs, newListOfYs)                 
   
  

updateKids : PlayerActivity -> Float -> List Kid -> {kids : List Kid, kidsMessages: List (Cmd Msg)}
updateKids playerActivity deltaSeconds kids =
  let
    (updatedKids, kidsMessages) =
      List.map (updateSingleKid playerActivity deltaSeconds) kids
      |> (List.foldr --transorm the list of pairs into pair of lists & handle the maybe's
          addPairWithMaybeToListOfPairs
          ([],[]))
  in
    {kids = updatedKids, kidsMessages = kidsMessages}



updateNerves : Float -> Model -> Float --return new nerves
updateNerves deltaSeconds model =
  let
    deltaPerSecond =
      case model.playerActivity of
        DeepBreath -> -gameConstants.deepBreathNervesRecovery
        CalmDownKid calmDownInfo -> 
          (List.filter (\kid -> kid.id == calmDownInfo.kidId) model.kids --the filtered list will always have one member, but it is easier to handle as a list
            |> List.map kidCalmDownActivityRecovery
            |> List.sum
          ) * gameConstants.calmDownNervesGrowthCoefficient   
        _ ->       
          -gameConstants.nervesBaseRecovery
  in
    defaultClamp ( model.nerves + deltaPerSecond * deltaSeconds )

updateActivity : Float -> Model -> Model
updateActivity deltaSeconds model =
    case model.playerActivity of
      CalmDownKid calmDownInfo ->
        let 
          newCalmDownDuration = calmDownInfo.duration + deltaSeconds 
        in
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
      updatedKids = 
        updateKids oldModel.playerActivity deltaSeconds oldModel.kids
    in
      
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
          , kids = updatedKids.kids
        }
      ! 
         updatedKids.kidsMessages
      

updateKidById : Int -> (Kid -> Kid) -> List Kid -> List Kid
updateKidById kidId updateFunction kids =
  let 
    mapFunction = \kid -> if kid.id == kidId then updateFunction kid else kid
  in
    List.map mapFunction kids

kidCalmDownFunction : Float -> Kid -> Kid
kidCalmDownFunction nerves kid =
  let 
    baseUpdatedKid =
      {kid |
          mutedCooldown = gameConstants.calmDownMutedTime
          , scheduledOutburst = emptyOutburstParams --reset outburst
          , shownPlayerDialog = Emojis.calmDown kid.activity
          , playerDialogCooldown = gameConstants.dialogCooldown
      }
  in
    if isKidAnnoying kid then
      {baseUpdatedKid |
        frustration =
            defaultClamp (
              kid.frustration
                + gameConstants.calmDownFrustrationGrowthMin
                + (
                  (gameConstants.calmDownFrustrationGrowthMax - gameConstants.calmDownFrustrationGrowthMin) 
                  * (nerves ^ gameConstants.calmDownFrustrationGrowthExponent)
                )
            )
      }
    else
      baseUpdatedKid

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

endActiveCalmDown : Model -> (Model, Cmd Msg)
endActiveCalmDown model =
    case model.playerActivity of
      CalmDownKid calmDownInfo ->
        { model | playerActivity = None }
        !
        (
          List.filter (\kid -> kid.id == calmDownInfo.kidId) model.kids   --reschedule outburst once calm down has ended
          |> List.map scheduleOutburstCmd          
        )
      _ ->  
       model ! []

processGameMessage : GameMessage -> Model -> (Model, Cmd Msg)
processGameMessage msg model =
  if not (shouldUpdateGame model) then (model, Cmd.none)
  else
    case msg of
      DeepBreathStarted ->
        let
          (newModel, cmd) = endActiveCalmDown model
        in
          (
            { newModel | playerActivity = DeepBreath }
            , cmd
          )
      DeepBreathEnded ->
        (
          if model.playerActivity == DeepBreath then
            { model | playerActivity = None}
          else model
        ) ! []
      CalmDownStarted kid ->
        let
          (newModel, cmd) = endActiveCalmDown model
        in
          (
            { model | 
              playerActivity = CalmDownKid { duration = 0, kidId = kid.id, nervesAtStart = model.nerves }
              , kids = updateKidById kid.id (kidCalmDownFunction model.nerves) model.kids 
            }
            , cmd
          )
      CalmDownEnded ->
        endActiveCalmDown model
      ScheduleOutburst outburstParams ->
        {model | 
          kids = updateKidById outburstParams.targetKidId (\kid -> {kid | scheduledOutburst = Debug.log "Outburst: " outburstParams}) model.kids
        }
        ! []



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
