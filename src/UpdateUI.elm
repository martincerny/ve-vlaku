module UpdateUI exposing (frame, message)

import Model
import UpdateGame
import Msg
import Init


frame : Float -> Model.Model -> Model.Model
frame deltaSeconds model =
    if model.transitionInactivity > 0 then
        { model | transitionInactivity = max 0 (model.transitionInactivity - deltaSeconds) }
    else
        model


message : Msg.UIMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
message msg model =
    if model.transitionInactivity > 0 then
        model ! []
    else
        (case msg of
            Msg.StartNewGame ->
                let
                    ( newGameModel, msgs ) =
                        Init.initGame

                    modelWithNewGame =
                        { model
                            | gameModel = newGameModel
                        }
                in
                    ((Model.setUIState modelWithNewGame Model.BeforeMission) , msgs)

            Msg.PauseMission ->
                (Model.setUIState model Model.PausedGame) ! []

            Msg.ResumeMission ->
                (Model.setUIState model Model.RunningGame) ! []

            Msg.ShowMainMenu ->
                (Model.setUIState model Model.MainMenu) ! []

            Msg.StartMission ->
                let
                    ( newGameModel, startMessages ) =
                        UpdateGame.startGame model.gameModel

                    newModel =
                        { model
                            | gameModel = newGameModel
                            , newlyAddedKids = []
                            , removedFrustratedKids = []
                            , removedKidsAfterMissionFail = []
                            , kidsWithReducedWaywardness = []
                        }
                in
                    (Model.setUIState newModel Model.RunningGame) ! startMessages

            Msg.SetScale scale ->
                { model | scale = scale } ! []
        )
