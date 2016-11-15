module UpdateMetaGame exposing (message, commandsForStateChange)

import Msg
import Model
import Random
import RandomGenerators
import GameConstants exposing (metaGameConstants)


commandsForStateChange : Model.GameState -> Model.GameModel -> List (Cmd Msg.Msg)
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
                [ Random.generate (Msg.metaGameMsg Msg.RemoveKidsAfterMissionFail) (RandomGenerators.removeKidsAfterMissionFail model)
                , Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin (List.length model.kids))
                ]

            _ ->
                []


message : Msg.MetaGameMessage -> Model.Model -> ( Model.Model, Cmd Msg.Msg )
message msg model =
    let
        gameModel =
            model.gameModel
    in
        (case msg of
            Msg.SetTimeToWin time ->
                { model | gameModel = { gameModel | timeToWin = time } }

            Msg.AddKids newKids ->
                let
                    newKidsWithIds =
                        List.indexedMap (\order kid -> { kid | id = gameModel.nextKidId + order }) newKids
                in
                    { model
                        | gameModel =
                            { gameModel
                                | kids = gameModel.kids ++ newKidsWithIds
                                , numKidsAdded = gameModel.numKidsAdded + List.length newKids
                                , nextKidId = gameModel.nextKidId + (List.length newKids)
                            }
                        , newlyAddedKids = newKidsWithIds
                    }

            Msg.RemoveFrustratedKids kidsToRemove ->
                let
                    newLength =
                        (List.length gameModel.kids) - (List.length kidsToRemove)

                    kidsToRemoveChecked =
                        if newLength >= metaGameConstants.minKidsToKeep then
                            kidsToRemove
                        else
                            List.take (List.length gameModel.kids - metaGameConstants.minKidsToKeep) kidsToRemove

                    idsToRemove =
                        List.map .id kidsToRemoveChecked
                in
                    { model
                        | gameModel =
                            { gameModel
                                | kids = List.filter (\kid -> not (List.member kid.id idsToRemove)) gameModel.kids
                                , numKidsRemoved = gameModel.numKidsRemoved + List.length kidsToRemoveChecked
                            }
                        , removedFrustratedKids = kidsToRemoveChecked
                    }

            Msg.RemoveKidsAfterMissionFail kidsToRemove ->
                let
                    newLength =
                        (List.length gameModel.kids) - (List.length kidsToRemove)

                    kidsToRemoveChecked =
                        if newLength >= metaGameConstants.minKidsToKeep then
                            kidsToRemove
                        else
                            List.take (List.length gameModel.kids - metaGameConstants.minKidsToKeep) kidsToRemove

                    idsToRemove =
                        List.map .id kidsToRemoveChecked
                in
                    { model
                        | gameModel =
                            { gameModel
                                | kids = List.filter (\kid -> not (List.member kid.id idsToRemove)) gameModel.kids
                                , numKidsRemoved = gameModel.numKidsRemoved + List.length kidsToRemoveChecked
                            }
                        , removedKidsAfterMissionFail = kidsToRemoveChecked
                    }

            Msg.ReduceWaywardness kidsToReduce ->
                let
                    idsToReduce =
                        List.map .id kidsToReduce
                in
                    { model
                        | gameModel =
                            { gameModel
                                | kids =
                                    gameModel.kids
                                        |> List.map
                                            (\kid ->
                                                if List.member kid.id idsToReduce then
                                                    { kid | waywardness = max metaGameConstants.minimalWaywardness (kid.waywardness - metaGameConstants.waywardnessReduction) }
                                                else
                                                    kid
                                            )
                                , numKidsReducedWaywardness = gameModel.numKidsReducedWaywardness + List.length kidsToReduce
                            }
                        , kidsWithReducedWaywardness = kidsToReduce
                    }
        )
            ! []
