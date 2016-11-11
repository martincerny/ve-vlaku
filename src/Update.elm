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
            UpdateGame.message gameMsg model

        Msg.Meta metaMsg ->
            UpdateMetaGame.message metaMsg model

        Msg.Frame delta ->
            let
                deltaSeconds =
                    delta / Time.second
            in
                if Model.shouldUpdateGame model then
                    UpdateGame.frame deltaSeconds model
                else
                    (UpdateUI.frame deltaSeconds model) ! []
