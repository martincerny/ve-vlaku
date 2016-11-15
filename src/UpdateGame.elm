module UpdateGame exposing (frame, message, startGame, beforeMission)

import Msg
import Model
import GameConstants exposing (gameConstants)
import UpdateUtils
import UpdateKids
import UpdateMetaGame
import RandomGenerators
import Random


--return new nerves


updateNervesTarget : Float -> Model.GameModel -> Float
updateNervesTarget deltaSeconds model =
    let
        deltaPerSecond =
            case model.playerActivity of
                Model.DeepBreath ->
                    -gameConstants.deepBreathNervesRecovery

                _ ->
                    -gameConstants.nervesBaseRecovery
    in
        UpdateUtils.defaultClamp (model.nervesTarget + deltaPerSecond * deltaSeconds)


updateActivity : Float -> Model.GameModel -> Model.GameModel
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


frame : Float -> Model.GameModel -> ( Model.GameModel, Cmd Msg.Msg )
frame deltaSeconds oldModel =
    let
        model =
            updateActivity deltaSeconds oldModel

        newState =
            (if oldModel.highActivityScore >= gameConstants.highActivityScoreToLose then
                Model.Lost Model.Activity
             else if oldModel.nerves >= 1 then
                Model.Lost Model.Nerves
             else if not (Model.isStateLost oldModel.state) && oldModel.timeToWin <= 0 then
                Model.Won
             else
                oldModel.state
            )

        updatedKids =
            UpdateKids.update oldModel.playerActivity deltaSeconds oldModel.kids
    in
        { model
            | state = newState
            , nervesTarget = updateNervesTarget deltaSeconds model
            , nerves = UpdateUtils.followTargetValue model.nervesTarget gameConstants.nervesTargetFollowingHalfTime deltaSeconds model.nerves
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
            ! updatedKids.kidsMessages


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


endActiveCalmDown : Model.GameModel -> ( Model.GameModel, Cmd Msg.Msg )
endActiveCalmDown model =
    case model.playerActivity of
        Model.CalmDownKid calmDownInfo ->
            { model | playerActivity = Model.None }
                ! (List.filter (\kid -> kid.id == calmDownInfo.kidId) model.kids
                    --reschedule outburst once calm down has ended
                    |>
                        List.map UpdateKids.scheduleOutburstCmd
                  )

        _ ->
            model ! []


message : Msg.GameMessage -> Model.GameModel -> ( Model.GameModel, Cmd Msg.Msg )
message msg model =
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
                    | playerActivity = Model.CalmDownKid { duration = 0, kidId = kid.id, positionId = kid.positionId, nervesAtStart = model.nerves }
                    , kids = updateKidById kid.id (UpdateKids.calmDownFunction model.nerves) model.kids
                    , nervesTarget = UpdateUtils.defaultClamp (newModel.nervesTarget + kid.activity * gameConstants.calmDownNervesGrowthCoefficient)
                  }
                , cmd
                )

        Msg.CalmDownEnded ->
            endActiveCalmDown model

        Msg.ScheduleOutburst outburstParams ->
            { model
                | kids = updateKidById outburstParams.targetKidId (UpdateKids.scheduleOutburst outburstParams) model.kids
            }
                ! []

        Msg.ScheduleFrustrationRecovery targetKid time ->
            { model
                | kids = updateKidById targetKid.id (\kid -> { kid | frustrationRecoveryEvent = Model.Scheduled time }) model.kids
            }
                ! []

        Msg.InitFrustration targetKid frustration ->
            { model
                | kids = updateKidById targetKid.id (\kid -> { kid | frustration = frustration }) model.kids
            }
                ! []

        Msg.InitActivity targetKid activity ->
            { model
                | kids = updateKidById targetKid.id (\kid -> { kid | activity = activity }) model.kids
            }
                ! []


startGame : Model.GameModel -> ( Model.GameModel, List (Cmd Msg.Msg) )
startGame model =
    ( { model
        | nerves = 0.1
        , nervesTarget = 0.1
        , kids = List.indexedMap UpdateKids.startGame model.kids
        , playerActivity = Model.None
        , highActivityScore = 0
        , state = Model.Running
      }
    , (List.map
        (\kid ->
            Random.generate (Msg.gameMsg (Msg.InitFrustration kid)) (RandomGenerators.frustrationInit kid)
        )
        model.kids
      )
        ++ (List.map
                (\kid ->
                    Random.generate (Msg.gameMsg (Msg.InitActivity kid)) (RandomGenerators.activityInit kid)
                )
                model.kids
           )
    )


beforeMission : Model.GameModel -> Cmd Msg.Msg
beforeMission model =
    Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin (List.length model.kids))
