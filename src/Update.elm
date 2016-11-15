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
            UpdateMetaGame.message metaMsg model

        Msg.Frame delta ->
            let
                deltaSeconds =
                    delta / Time.second
            in
                if Model.shouldUpdateGame model then
                    let
                        (updatedGameModel, cmd) =
                            UpdateGame.frame deltaSeconds model.gameModel
                    in
                        (
                        { model
                            | gameModel = updatedGameModel
                            , uiState =
                                case updatedGameModel.state of
                                    Model.Lost _ ->
                                        Model.MissionSummary

                                    Model.Won ->
                                        Model.MissionSummary

                                    _ ->
                                        model.uiState
                        }, cmd )
                    
                else
                    (UpdateUI.frame deltaSeconds model) ! []
