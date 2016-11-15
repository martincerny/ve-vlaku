module RandomGenerators
    exposing
        ( outburstParams
        , addKidAfterWin
        , timeToWin
        , frustrationRecoveryInterval
        , removeKidsWithBadMood
        , removeKidsAfterMissionFail
        , reduceWaywardness
        , frustrationInit
        , activityInit
        )

import GameConstants exposing (..)
import Random
import Model
import KidGenerator
import Utils
import RandomUtils



outburstInterval : Float -> Random.Generator Float
outburstInterval waywardness =
    let
        minMeanInterval =
            gameConstants.meanOutburstIntervalMin

        maxMeanInterval =
            gameConstants.meanOutburstIntervalMax

        actualMeanInterval =
            (minMeanInterval + (1 - waywardness) * (maxMeanInterval - minMeanInterval))
    in
        RandomUtils.exponentialGenerator gameConstants.minOutburstInterval actualMeanInterval


outburstIntensity : Random.Generator Float
outburstIntensity =
    Random.float 0 1


outburstParams : Int -> Float -> Random.Generator Model.OutburstParams
outburstParams kidId waywardness =
    Random.map2
        (\interval intensity -> { targetKidId = kidId, interval = interval, intensity = intensity })
        (outburstInterval waywardness)
        outburstIntensity


frustrationRecoveryInterval : Random.Generator Float
frustrationRecoveryInterval =
    RandomUtils.exponentialGenerator gameConstants.calmDownFrustrationRecoveryMinInterval gameConstants.calmDownFrustrationRecoveryMeanInterval


numKidsToBeAddedAfterWin : Model.GameModel -> Float -> Int
numKidsToBeAddedAfterWin model randomValue =
    let
        meanFrustration =
            model.kids |> List.map .frustration |> Utils.avg
    in
        if meanFrustration < metaGameConstants.maximalMeanFrustrationToAddKid then
            let
                baseChance =
                    (1 - (meanFrustration / metaGameConstants.maximalMeanFrustrationToAddKid))

                oneKidChance =
                    baseChance * metaGameConstants.maximalChanceOfAddingKid

                twoKidChance =
                    baseChance * metaGameConstants.maximalChanceOfAddingTwoKids
            in
                if randomValue <= twoKidChance then
                    2
                else if randomValue <= oneKidChance then
                    1
                else
                    0
        else
            0


addKidAfterWin : Model.GameModel -> Random.Generator (List Model.Kid)
addKidAfterWin model =
    let
        numKidsGenerator =
            Random.map (numKidsToBeAddedAfterWin model) (Random.float 0 1)
    in
        Random.andThen (numKidsGenerator)
            (\numKids ->
                Random.list numKids KidGenerator.generator
            )


shouldKidBeRemovedForBadMood : Model.Kid -> Random.Generator Bool
shouldKidBeRemovedForBadMood kid =
    if kid.frustration > metaGameConstants.minFrustrationToConsiderRemovingKid then
        let
            chanceToRemove =
                ((kid.frustration - metaGameConstants.minFrustrationToConsiderRemovingKid) / (1 - metaGameConstants.minFrustrationToConsiderRemovingKid))
                    * metaGameConstants.maxChanceOfRemovingKidForFrustration
        in
            Random.map (\x -> x < chanceToRemove) (Random.float 0 1)
    else
        RandomUtils.fixedGenerator False


removeKidsWithBadMood : Model.GameModel -> Random.Generator (List Model.Kid)
removeKidsWithBadMood model =
    let
        listOfChoicesGenerator =
            model.kids
                |> List.map shouldKidBeRemovedForBadMood
                |> RandomUtils.listOfGeneratorsToGeneratorOfList
    in
        Random.map (\boolList -> Utils.chooseByBoolList boolList model.kids) listOfChoicesGenerator


numKidsToBeRemovedAfterMissionFail : Model.GameModel -> Random.Generator Int
numKidsToBeRemovedAfterMissionFail model =
    Random.float 0 ((toFloat (List.length model.kids)) / metaGameConstants.numKidsFor50PercentChanceRemovalAfterMissionFail)
        |> Random.map round



--the lower the waywardness, the more likely is the kid to be chosen


chooseKidsByWaywardnessTarget : List Model.Kid -> List Float -> List Model.Kid
chooseKidsByWaywardnessTarget kids targets =
    case kids of
        head :: tail ->
            let
                reducedTargets =
                    List.map (\x -> x - (1 - head.waywardness)) targets
                        |> List.filter (\x -> x >= 0)

                recursiveResult =
                    chooseKidsByWaywardnessTarget tail reducedTargets
            in
                if List.any (\x -> x < (1 - head.waywardness)) targets then
                    head :: recursiveResult
                else
                    recursiveResult

        [] ->
            []


removeKidsAfterMissionFail : Model.GameModel -> Random.Generator (List Model.Kid)
removeKidsAfterMissionFail model =
    let
        waywardnessSum =
            model.kids |> List.map (\x -> 1 - x.waywardness) |> List.sum

        targetWaywardness =
            Random.andThen (numKidsToBeRemovedAfterMissionFail model) (\n -> Random.list n (Random.float 0 waywardnessSum))
    in
        Random.map (chooseKidsByWaywardnessTarget model.kids) targetWaywardness


shouldKidHaveWaywardnessReduced : Model.Kid -> Random.Generator Bool
shouldKidHaveWaywardnessReduced kid =
    if
        kid.frustration
            < metaGameConstants.maxFrustrationToConsiderReducingWaywardness
            && kid.waywardness
            > metaGameConstants.minimalWaywardness
    then
        let
            chanceToReduce =
                ((metaGameConstants.maxFrustrationToConsiderReducingWaywardness - kid.frustration) / (metaGameConstants.maxFrustrationToConsiderReducingWaywardness))
                    * metaGameConstants.maxChanceOfReducingWaywardness
        in
            Random.map (\x -> x < chanceToReduce) (Random.float 0 1)
    else
        RandomUtils.fixedGenerator False


reduceWaywardness : Model.GameModel -> Random.Generator (List Model.Kid)
reduceWaywardness model =
    let
        listOfChoicesGenerator =
            model.kids
                |> List.map shouldKidHaveWaywardnessReduced
                |> RandomUtils.listOfGeneratorsToGeneratorOfList
    in
        Random.map (\boolList -> Utils.chooseByBoolList boolList model.kids) listOfChoicesGenerator


timeToWin : Int -> Random.Generator Float
timeToWin numKids =
    let
        minimum =
            max metaGameConstants.minTimeToWin ((toFloat numKids) * metaGameConstants.minTimeToWinPerKid)

        maximum =
            (toFloat numKids) * metaGameConstants.maxTimeToWinPerKid
    in
        Random.float minimum maximum


frustrationInit : Model.Kid -> Random.Generator Float
frustrationInit _ =
    Random.float 0 1


activityInit : Model.Kid -> Random.Generator Float
activityInit kid =
    Random.float 0 (min (gameConstants.highActivityThreshold - 0.1) kid.waywardness)
