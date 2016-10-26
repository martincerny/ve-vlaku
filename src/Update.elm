module Update exposing (update)

import Time
import Msg 
import Model 
import GameConstants exposing (gameConstants)
import Init
import Emojis
import Random
import RandomGenerators
import Debug


defaultClamp : Float -> Float
defaultClamp =
    clamp 0 1


updateKidDialogs : Float -> Model.Kid -> Model.Kid
updateKidDialogs deltaSeconds kid =
    { kid
        | kidDialogCooldown = max (kid.kidDialogCooldown - deltaSeconds) 0
        , playerDialogCooldown = max (kid.playerDialogCooldown - deltaSeconds) 0
    }


scheduleOutburstCmd : Model.Kid -> Cmd Msg.Msg
scheduleOutburstCmd kid =
    Random.generate (Msg.gameMsg Msg.ScheduleOutburst) (RandomGenerators.outburstParams kid.id kid.waywardness)


updateKidOutburst : Model.Kid -> ( Model.Kid, Maybe (Cmd Msg.Msg) )
updateKidOutburst kid =
    let
        outburstActive =
            Model.isActiveOutburst kid.scheduledOutburst
    in
        if kid.timeSinceLastOutburst < kid.scheduledOutburst.interval && outburstActive then
            ( kid, Nothing )
        else
            let
                minGrowth =
                    gameConstants.outburstMinActivityGrowth

                maxGrowth =
                    gameConstants.outburstMaxActivityGrowth

                intensity =
                    kid.waywardness * kid.scheduledOutburst.intensity
            in
                ( if not outburstActive then
                    kid
                  else
                    { kid
                        | activity = defaultClamp (kid.activity + minGrowth + (intensity * (maxGrowth - minGrowth)))
                        , shownKidDialog = Emojis.outburst intensity
                        , kidDialogCooldown = gameConstants.dialogCooldown
                        , timeSinceLastOutburst = 0
                        , scheduledOutburst = Model.emptyOutburstParams
                    }
                , Just (scheduleOutburstCmd kid)
                )


updateKidDefault : Float -> Model.Kid -> ( Model.Kid, Maybe (Cmd Msg.Msg) )
updateKidDefault deltaSeconds kid =
    let
        usefulDelta =
            if deltaSeconds < kid.mutedCooldown then
                0
            else
                deltaSeconds - kid.mutedCooldown

        frustrationThreshold =
            gameConstants.activityFrustrationGrowthThreshold
    in
        { kid
            | activity =
                defaultClamp
                    (kid.activity
                        + usefulDelta
                        * kid.waywardness
                        * (gameConstants.activityBaseGrowth
                            + if kid.frustration > frustrationThreshold then
                                gameConstants.activityFrustrationGrowth
                                    * ((kid.frustration - frustrationThreshold) / (1 - frustrationThreshold))
                              else
                                0
                          )
                    )
            , frustration = max (kid.frustration - usefulDelta * gameConstants.frustrationRecovery) 0
            , timeSinceLastOutburst =
                kid.timeSinceLastOutburst + usefulDelta
                --outbursts are not ticking when the kid is muted, so they use usefulDelta
            , mutedCooldown =
                max (kid.mutedCooldown - deltaSeconds) 0
                --mutedCooldown uses deltaSeconds because those timers should work even when the kid is muted
        }
            |> (updateKidDialogs deltaSeconds)
            |> updateKidOutburst


calmDownFrustrationRecovery : Float -> Float -> Float
calmDownFrustrationRecovery calmDownDuration oldFrustration =
    if calmDownDuration <= gameConstants.calmDownFrustrationRecoveryStart then
        0
    else
        gameConstants.calmDownDurationFrustrationRecovery


kidCalmDownActivityRecovery : Model.Kid -> Float
kidCalmDownActivityRecovery kid =
    (kid.activity / (2 * gameConstants.calmDownActivityRecoveryHalfTime))


updateKidCalmDown : Model.CalmDownInfo -> Float -> Model.Kid -> ( Model.Kid, Maybe (Cmd Msg.Msg) )
updateKidCalmDown calmDownInfo deltaSeconds kid =
    let
        frustrationRecoveryStarts =
            (calmDownInfo.duration < gameConstants.calmDownFrustrationRecoveryStart)
                && (calmDownInfo.duration + deltaSeconds >= gameConstants.calmDownFrustrationRecoveryStart)

        newPlayerDialog =
            if frustrationRecoveryStarts then
                Emojis.frustrationRecovery kid.frustration
            else
                kid.shownPlayerDialog

        newPlayerDialogCooldown =
            if frustrationRecoveryStarts then
                gameConstants.dialogCooldown
            else
                kid.playerDialogCooldown
    in
        ( { kid
            | activity = defaultClamp (kid.activity - deltaSeconds * (kidCalmDownActivityRecovery kid))
            , frustration = defaultClamp (kid.frustration - deltaSeconds * (calmDownFrustrationRecovery calmDownInfo.duration kid.frustration))
            , mutedCooldown =
                gameConstants.calmDownMutedTime
                --always reset the cooldown
            , shownPlayerDialog = newPlayerDialog
            , playerDialogCooldown = newPlayerDialogCooldown
          }
            |> (updateKidDialogs deltaSeconds)
        , Nothing
        )


updateSingleKid : Model.PlayerActivity -> Float -> Model.Kid -> ( Model.Kid, Maybe (Cmd Msg.Msg) )
updateSingleKid playerActivity deltaSeconds kid =
    case playerActivity of
        Model.CalmDownKid calmDownInfo ->
            if calmDownInfo.kidId == kid.id then
                updateKidCalmDown calmDownInfo deltaSeconds kid
            else
                updateKidDefault deltaSeconds kid

        _ ->
            updateKidDefault deltaSeconds kid


addPairWithMaybeToListOfPairs : ( a, Maybe b ) -> ( List a, List b ) -> ( List a, List b )
addPairWithMaybeToListOfPairs ( x, maybeY ) ( listOfXs, listOfYs ) =
    let
        newListOfYs =
            case maybeY of
                Just y ->
                    y :: listOfYs

                Nothing ->
                    listOfYs
    in
        ( x :: listOfXs, newListOfYs )


updateKids : Model.PlayerActivity -> Float -> List Model.Kid -> { kids : List Model.Kid, kidsMessages : List (Cmd Msg.Msg) }
updateKids playerActivity deltaSeconds kids =
    let
        ( updatedKids, kidsMessages ) =
            List.map (updateSingleKid playerActivity deltaSeconds) kids
                |> (List.foldr
                        --transorm the list of pairs into pair of lists & handle the maybe's
                        addPairWithMaybeToListOfPairs
                        ( [], [] )
                   )
    in
        { kids = updatedKids, kidsMessages = kidsMessages }


--return new nerves
updateNerves : Float -> Model.Model -> Float
updateNerves deltaSeconds model =
    let
        deltaPerSecond =
            case model.playerActivity of
                Model.DeepBreath ->
                    -gameConstants.deepBreathNervesRecovery

                Model.CalmDownKid calmDownInfo ->
                    (List.filter (\kid -> kid.id == calmDownInfo.kidId) model.kids
                        --the filtered list will always have one member, but it is easier to handle as a list
                        |>
                            List.map kidCalmDownActivityRecovery
                        |> List.sum
                    )
                        * gameConstants.calmDownNervesGrowthCoefficient

                _ ->
                    -gameConstants.nervesBaseRecovery
    in
        defaultClamp (model.nerves + deltaPerSecond * deltaSeconds)


updateActivity : Float -> Model.Model -> Model.Model
updateActivity deltaSeconds model =
    case model.playerActivity of
        Model.CalmDownKid calmDownInfo ->
            let
                newCalmDownDuration =
                    calmDownInfo.duration + deltaSeconds
            in
                { model
                    | playerActivity = Model.CalmDownKid { calmDownInfo | duration = newCalmDownDuration }
                }

        --ignoring deepBreath here as it is handled in updateNerves
        _ ->
            model


commandsForStateChange: Model.GameState -> Model.Model -> List (Cmd Msg.Msg)
commandsForStateChange oldState model =
    if oldState == model.state then
        []
    else
        case model.state of 
            Model.Won ->
                [
                    Random.generate (Msg.metaGameMsg Msg.AddKids) (RandomGenerators.addKidAfterWin model)
                    , Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin (List.length model.kids))
                ]
            _ ->
                []


updateGameFrame : Float -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
updateGameFrame deltaSeconds oldModel =
    let
        model =
            Model.setState oldModel
                (if oldModel.highActivityScore >= gameConstants.highActivityScoreToLose then
                    Model.Lost Model.Activity
                 else if oldModel.nerves >= 1 then
                    Model.Lost Model.Nerves
                 else if not (Model.isStateLost oldModel.state) && oldModel.timeToWin <= 0 then
                    Model.Won
                 else
                    oldModel.state
                )
             |> updateActivity deltaSeconds

        additionalCommands =
            commandsForStateChange oldModel.state model                

        updatedKids =
            updateKids oldModel.playerActivity deltaSeconds oldModel.kids
    in
        { model
            | nerves = updateNerves deltaSeconds model
            , highActivityScore =
                let
                    numHighActivityKids =
                        List.filter Model.isKidHighActivity model.kids |> List.length
                in
                    if numHighActivityKids > 0 then
                        model.highActivityScore + deltaSeconds * (toFloat numHighActivityKids) * gameConstants.highActivityScoreIncreasePerKid
                    else
                        max 0 (model.highActivityScore - deltaSeconds * gameConstants.highActivityScoreRecovery)
            , timeToWin = max 0 (model.timeToWin - deltaSeconds)
            , kids = updatedKids.kids
        }
            ! (
                updatedKids.kidsMessages
                ++ additionalCommands ) 


updateKidById : Int -> (Model.Kid -> Model.Kid) -> List Model.Kid -> List Model.Kid
updateKidById kidId updateFunction kids =
    let
        mapFunction =
            \kid ->
                if kid.id == kidId then
                    updateFunction kid
                else
                    kid
    in
        List.map mapFunction kids


kidCalmDownFunction : Float -> Model.Kid -> Model.Kid
kidCalmDownFunction nerves kid =
    let
        baseUpdatedKid =
            { kid
                | mutedCooldown = gameConstants.calmDownMutedTime
                , scheduledOutburst =
                    Model.emptyOutburstParams
                    --reset outburst
                , shownPlayerDialog = Emojis.calmDown kid.activity
                , playerDialogCooldown = gameConstants.dialogCooldown
            }
    in
        if Model.isKidAnnoying kid then
            { baseUpdatedKid
                | frustration =
                    defaultClamp
                        (kid.frustration
                            + gameConstants.calmDownFrustrationGrowthMin
                            + ((gameConstants.calmDownFrustrationGrowthMax - gameConstants.calmDownFrustrationGrowthMin)
                                * (nerves ^ gameConstants.calmDownFrustrationGrowthExponent)
                              )
                        )
            }
        else
            baseUpdatedKid


updateUIFrame : Float -> Model.Model -> Model.Model
updateUIFrame deltaSeconds model =
    if model.transitionInactivity > 0 then
        { model | transitionInactivity = max 0 (model.transitionInactivity - deltaSeconds) }
    else
        model

startGame : Model.Model -> Model.Model
startGame model=
    {model | 
        kids = List.map (\kid -> {kid | activity = 0.5 * kid.waywardness }) model.kids
        , playerActivity = Model.None
        , newlyAddedKids = []
        , firstRun = False
    }

processUIMessage : Msg.UIMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
processUIMessage msg model =
    if model.transitionInactivity > 0 then
        model ! []
    else
        (case msg of
            Msg.ResumeGame ->
                (Model.setState (startGame model) Model.Running) ! []
            Msg.RestartGame ->
                Init.init
        )


endActiveCalmDown : Model.Model -> ( Model.Model, Cmd Msg.Msg )
endActiveCalmDown model =
    case model.playerActivity of
        Model.CalmDownKid calmDownInfo ->
            { model | playerActivity = Model.None }
                ! (List.filter (\kid -> kid.id == calmDownInfo.kidId) model.kids
                    --reschedule outburst once calm down has ended
                    |>
                        List.map scheduleOutburstCmd
                  )

        _ ->
            model ! []


processGameMessage : Msg.GameMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
processGameMessage msg model =
    if not (Model.shouldUpdateGame model) then
        ( model, Cmd.none )
    else
        case msg of
            Msg.DeepBreathStarted ->
                let
                    ( newModel, cmd ) =
                        endActiveCalmDown model
                in
                    ( { newModel | playerActivity = Model.DeepBreath }
                    , cmd
                    )

            Msg.DeepBreathEnded ->
                (if model.playerActivity == Model.DeepBreath then
                    { model | playerActivity = Model.None }
                 else
                    model
                )
                    ! []

            Msg.CalmDownStarted kid ->
                let
                    ( newModel, cmd ) =
                        endActiveCalmDown model
                in
                    ( { model
                        | playerActivity = Model.CalmDownKid { duration = 0, kidId = kid.id, nervesAtStart = model.nerves }
                        , kids = updateKidById kid.id (kidCalmDownFunction model.nerves) model.kids
                      }
                    , cmd
                    )

            Msg.CalmDownEnded ->
                endActiveCalmDown model

            Msg.ScheduleOutburst outburstParams ->
                { model
                    | kids = updateKidById outburstParams.targetKidId (\kid -> { kid | scheduledOutburst = outburstParams }) model.kids
                }
                    ! []

processMetaGameMessage : Msg.MetaGameMessage -> Model.Model -> (Model.Model, Cmd Msg.Msg)
processMetaGameMessage msg model =
    (
    case msg of
        Msg.SetTimeToWin time ->
            {model | timeToWin = time}
        Msg.AddKids newKids ->
            let
              newKidsWithIds = List.indexedMap (\order kid -> {kid | id = model.nextKidId + order}) newKids
            in              
                {model |
                    kids = model.kids ++ newKidsWithIds
                    , nextKidId = model.nextKidId + (List.length newKids)
                    , newlyAddedKids = newKidsWithIds
                }
        _ ->
            Debug.crash "Unexpected message" model
    ) ! []

update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.UI uiMsg ->
            processUIMessage uiMsg model

        Msg.Game gameMsg ->
            processGameMessage gameMsg model

        Msg.Meta metaMsg ->
            processMetaGameMessage metaMsg model

        Msg.Frame delta ->
            let
                deltaSeconds =
                    delta / Time.second
            in
                if Model.shouldUpdateGame model then
                    updateGameFrame deltaSeconds model
                else
                    (updateUIFrame deltaSeconds model) ! []
