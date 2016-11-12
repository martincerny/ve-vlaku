module UpdateMetaGame exposing (message, commandsForStateChange)

import Msg
import Model
import Random
import RandomGenerators
import GameConstants exposing (metaGameConstants)


commandsForStateChange : Model.GameState -> Model.Model -> List (Cmd Msg.Msg)
commandsForStateChange oldState model =
    if oldState == model.state then
        []
    else
        case model.state of
            Model.Won ->
                [ Random.generate (Msg.metaGameMsg Msg.AddKids) (RandomGenerators.addKidAfterWin model)
                , Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin (List.length model.kids))
                , Random.generate (Msg.metaGameMsg Msg.RemoveFrustratedKids) (RandomGenerators.removeKidsWithBadMood model)
                , Random.generate (Msg.metaGameMsg Msg.ReduceWaywardness) (RandomGenerators.reduceWaywardness model)
                ]

            Model.Lost _ ->
                [
                    Random.generate (Msg.metaGameMsg Msg.RemoveKidsAfterMissionFail) (RandomGenerators.removeKidsAfterMissionFail model)
                ]
            _ ->
                []


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

        Msg.RemoveFrustratedKids kidsToRemove ->
            let
                newLength =
                    (List.length model.kids) - (List.length kidsToRemove)

                kidsToRemoveChecked =
                    if newLength >= metaGameConstants.minKidsToKeep then
                        kidsToRemove
                    else
                        List.take (List.length model.kids - metaGameConstants.minKidsToKeep) kidsToRemove

                idsToRemove =
                    List.map .id kidsToRemoveChecked
            in
                { model
                    | kids = List.filter (\kid -> not (List.member kid.id idsToRemove)) model.kids
                    , removedFrustratedKids = kidsToRemoveChecked
                }

        Msg.RemoveKidsAfterMissionFail kidsToRemove ->
            let
                newLength =
                    (List.length model.kids) - (List.length kidsToRemove)

                kidsToRemoveChecked =
                    if newLength >= metaGameConstants.minKidsToKeep then
                        kidsToRemove
                    else
                        List.take (List.length model.kids - metaGameConstants.minKidsToKeep) kidsToRemove

                idsToRemove =
                    List.map .id kidsToRemoveChecked
            in
                { model
                    | kids = List.filter (\kid -> not (List.member kid.id idsToRemove)) model.kids
                    , removedKidsAfterMissionFail = kidsToRemoveChecked
                }

        Msg.ReduceWaywardness kidsToReduce ->
            let
                idsToReduce =
                    List.map .id kidsToReduce
            in
                { model
                    | kids =
                        model.kids
                        |> List.map
                            (\kid ->
                                if List.member kid.id idsToReduce then
                                    { kid | waywardness = max metaGameConstants.minimalWaywardness (kid.waywardness - metaGameConstants.waywardnessReduction) }
                                else
                                    kid
                            )
                }
    )
        ! []
