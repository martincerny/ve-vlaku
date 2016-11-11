module UpdateUI exposing (frame, message)

import Model
import Msg
import Init


frame : Float -> Model.Model -> Model.Model
frame deltaSeconds model =
    if model.transitionInactivity > 0 then
        { model | transitionInactivity = max 0 (model.transitionInactivity - deltaSeconds) }
    else
        model


startGame : Model.Model -> Model.Model
startGame model =
    { model
        | kids = List.map (\kid -> { kid | activity = 0.5 * kid.waywardness }) model.kids
        , playerActivity = Model.None
        , newlyAddedKids = []
        , firstRun = False
    }


message : Msg.UIMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
message msg model =
    if model.transitionInactivity > 0 then
        model ! []
    else
        (case msg of
            Msg.ResumeGame ->
                (Model.setState (startGame model) Model.Running) ! []

            Msg.RestartGame ->
                Init.init
        )
