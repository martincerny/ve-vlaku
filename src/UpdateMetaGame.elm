module UpdateMetaGame exposing (message)

import Msg
import Model


message : Msg.MetaGameMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
message msg model =
    (case msg of
        Msg.SetTimeToWin time ->
            { model | timeToWin = time }

        Msg.AddKids newKids ->
            let
                newKidsWithIds =
                    List.indexedMap (\order kid -> { kid | id = model.nextKidId + order }) newKids
            in
                { model
                    | kids = model.kids ++ newKidsWithIds
                    , nextKidId = model.nextKidId + (List.length newKids)
                    , newlyAddedKids = newKidsWithIds
                }

        _ ->
            Debug.crash "Unexpected message" model
    )
        ! []
