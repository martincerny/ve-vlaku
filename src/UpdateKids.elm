module UpdateKids
    exposing
        ( update
        , calmDownFunction
        , scheduleOutburstCmd
        , startGame
        , scheduleOutburst
        )

import Model
import Msg
import Random
import RandomGenerators
import GameConstants exposing (..)
import UpdateUtils
import Emojis


calmDownFunction : Float -> Model.Kid -> Model.Kid
calmDownFunction nerves kid =
    let
        baseUpdatedKid =
            { kid
                | mutedCooldown = gameConstants.calmDownMutedTime
                , scheduledOutburst =
                    Model.emptyOutburstParams
                    --reset outburst
                , frustrationRecoveryEvent = Model.Scheduled gameConstants.calmDownFrustrationRecoveryStart
                , shownPlayerDialog = Emojis.calmDown kid.activity
                , playerDialogCooldown = gameConstants.dialogCooldown
                , activity = 0
                , numCalmDowns = kid.numCalmDowns + 1
            }
    in
        if Model.isKidAnnoying kid then
            { baseUpdatedKid
                | frustration =
                    UpdateUtils.defaultClamp
                        (kid.frustration
                            + gameConstants.calmDownFrustrationGrowth
                        )
            }
        else
            baseUpdatedKid


updateKidDialogs : Float -> Model.Kid -> Model.Kid
updateKidDialogs deltaSeconds kid =
    { kid
        | kidDialogCooldown = max (kid.kidDialogCooldown - deltaSeconds) 0
        , playerDialogCooldown = max (kid.playerDialogCooldown - deltaSeconds) 0
    }


scheduleOutburstCmd : Model.Kid -> Cmd Msg.Msg
scheduleOutburstCmd kid =
    Random.generate (Msg.gameMsg Msg.ScheduleOutburst) (RandomGenerators.outburstParams kid.id kid.waywardness)


scheduleOutburst : Model.OutburstParams -> Model.Kid -> Model.Kid
scheduleOutburst outburstParams kid =
    { kid
        | scheduledOutburst = outburstParams
        , numScheduledOutbursts = kid.numScheduledOutbursts + 1
        , sumOutburstIntervals = kid.sumOutburstIntervals + outburstParams.interval
    }


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
                        | activity = UpdateUtils.defaultClamp (kid.activity + minGrowth + (intensity * (maxGrowth - minGrowth)))
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
                UpdateUtils.defaultClamp
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


updateKidCalmDown : Model.CalmDownInfo -> Float -> Model.Kid -> ( Model.Kid, Maybe (Cmd Msg.Msg) )
updateKidCalmDown calmDownInfo deltaSeconds kid =
    let
        ( doFrustrationRecovery, newFrustrationRecoveryEvent ) =
            case kid.frustrationRecoveryEvent of
                Model.Unscheduled ->
                    ( False, Model.Unscheduled )

                Model.Scheduled time ->
                    if (time <= 0) then
                        ( True, Model.Unscheduled )
                    else
                        ( False, Model.Scheduled (time - deltaSeconds) )

        newFrustration =
            if doFrustrationRecovery then
                UpdateUtils.defaultClamp (kid.frustration - gameConstants.calmDownFrustrationRecovery)
            else
                kid.frustration

        newPlayerDialog =
            if doFrustrationRecovery then
                Emojis.frustrationRecovery kid.frustration
            else
                kid.shownPlayerDialog

        newPlayerDialogCooldown =
            if doFrustrationRecovery then
                gameConstants.dialogCooldown
            else
                kid.playerDialogCooldown

        msg =
            if doFrustrationRecovery then
                Just (Random.generate (Msg.gameMsg (Msg.ScheduleFrustrationRecovery kid)) RandomGenerators.frustrationRecoveryInterval)
            else
                Nothing
    in
        ( { kid
            | frustration = newFrustration
            , mutedCooldown =
                gameConstants.calmDownMutedTime
                --always reset the cooldown
            , shownPlayerDialog = newPlayerDialog
            , playerDialogCooldown = newPlayerDialogCooldown
            , frustrationRecoveryEvent = newFrustrationRecoveryEvent
          }
            |> (updateKidDialogs deltaSeconds)
        , msg
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


update : Model.PlayerActivity -> Float -> List Model.Kid -> { kids : List Model.Kid, kidsMessages : List (Cmd Msg.Msg) }
update playerActivity deltaSeconds kids =
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


startGame : Model.Kid -> Model.Kid
startGame kid =
    { kid
        | activity = 0
        , frustration = 0
        , mutedCooldown = 0
        , shownKidDialog = Emojis.nothing
        , kidDialogCooldown = 0
        , shownPlayerDialog = Emojis.nothing
        , playerDialogCooldown = 0
        , timeSinceLastOutburst = 0
        , scheduledOutburst = Model.emptyOutburstParams
        , frustrationRecoveryEvent = Model.Unscheduled
        , numCalmDowns = 0
        , numScheduledOutbursts = 0
        , sumOutburstIntervals = 0
    }
