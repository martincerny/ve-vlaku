module Update
    exposing
        ( update
        )

import Msg
import Model
import UpdateUI
import UpdateGame
import UpdateMetaGame
import Time
import SaveLoad


update : Msg.Msg -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
update msg model =
    case msg of
        Msg.UI uiMsg ->
            UpdateUI.message uiMsg model

        Msg.Game gameMsg ->
            let
                ( newGameModel, cmd ) =
                    UpdateGame.message gameMsg model.gameModel
            in
                ( { model | gameModel = newGameModel }, cmd )

        Msg.Meta metaMsg ->
            let (newModel, cmd) =
                UpdateMetaGame.message metaMsg model
            in 
                newModel ! [cmd, SaveLoad.saveGame (SaveLoad.encodeGame newModel.gameModel) ]

        Msg.Frame delta ->
            let
                deltaSeconds =
                    delta / Time.second
                
                updatedUIModel = UpdateUI.frame deltaSeconds model
            in
                if Model.shouldUpdateGame updatedUIModel then
                    let
                        ( updatedGameModel, cmd ) =
                            UpdateGame.frame deltaSeconds updatedUIModel.gameModel

                        ( missionEnded, numFailures ) =
                            case updatedGameModel.state of
                                Model.Lost _ ->
                                    ( True, 1 )

                                Model.Won ->
                                    ( True, 0 )

                                _ ->
                                    ( False, 0 )
                    in
                        if missionEnded then
                            { updatedUIModel
                                | gameModel =
                                    { updatedGameModel
                                        | numMissions = updatedGameModel.numMissions + 1
                                        , numFailures = updatedGameModel.numFailures + numFailures
                                    }
                                , uiState = Model.MissionSummary
                            }
                                ! (cmd :: UpdateMetaGame.commandsForMissionEnd updatedGameModel)
                        else
                            ( { updatedUIModel | gameModel = updatedGameModel }, cmd )
                else
                    updatedUIModel ! []
