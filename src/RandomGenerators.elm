module RandomGenerators
    exposing
        ( outburstParams
        , addKidAfterWin
        , timeToWin
        , frustrationRecoveryInterval
        , exponentialGenerator
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


fixedGenerator : a -> Random.Generator a
fixedGenerator value =
    --I do not consume the bool, but there is no way to create my own primitive generator
    Random.map (\_ -> value) Random.bool


listOfGeneratorsToGeneratorOfList : List (Random.Generator a) -> Random.Generator (List a)
listOfGeneratorsToGeneratorOfList listOfGenerators =
    case listOfGenerators of
        head :: tail ->
            Random.andThen
                (listOfGeneratorsToGeneratorOfList tail)
                (\list -> Random.map (\x -> x :: list) head)

        [] ->
            fixedGenerator []


exponentialInverseCDF : Float -> Float -> Float
exponentialInverseCDF mean y =
    -mean * logBase e (1 - y)


exponentialGenerator : Float -> Float -> Random.Generator Float
exponentialGenerator minimum mean =
    let
        distributionMean =
            mean - minimum
    in
        Random.map (exponentialInverseCDF distributionMean >> (+) minimum) (Random.float 0 1)


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
        exponentialGenerator gameConstants.minOutburstInterval actualMeanInterval


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
    exponentialGenerator gameConstants.calmDownFrustrationRecoveryMinInterval gameConstants.calmDownFrustrationRecoveryMeanInterval


shouldKidBeAddedAfterWin : Model.Model -> Float -> Bool
shouldKidBeAddedAfterWin model randomValue =
    let
        meanFrustration =
            model.kids |> List.map .frustration |> Utils.avg
    in
        if meanFrustration < metaGameConstants.maximalMeanFrustrationToAddKid then
            let
                chance =
                    (1 - (meanFrustration / metaGameConstants.maximalMeanFrustrationToAddKid))
                        * metaGameConstants.maximalChanceOfAddingKid
            in
                randomValue <= chance
        else
            False


addKidAfterWin : Model.Model -> Random.Generator (List Model.Kid)
addKidAfterWin model =
    let
        shouldBeAddedGenerator =
            Random.map (shouldKidBeAddedAfterWin model) (Random.float 0 1)
    in
        Random.andThen (shouldBeAddedGenerator)
            (\shouldBeAdded ->
                if shouldBeAdded then
                    Random.list 1 KidGenerator.generator
                else
                    fixedGenerator []
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
        fixedGenerator False


removeKidsWithBadMood : Model.Model -> Random.Generator (List Model.Kid)
removeKidsWithBadMood model =
    let
        listOfChoicesGenerator =
            model.kids
                |> List.map shouldKidBeRemovedForBadMood
                |> listOfGeneratorsToGeneratorOfList
    in
        Random.map (\boolList -> Utils.chooseByBoolList boolList model.kids) listOfChoicesGenerator


numKidsToBeRemovedAfterMissionFail : Model.Model -> Random.Generator Int
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


removeKidsAfterMissionFail : Model.Model -> Random.Generator (List Model.Kid)
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
    if kid.frustration < metaGameConstants.maxFrustrationToConsiderReducingWaywardness
     && kid.waywardness > metaGameConstants.minimalWaywardness then
        let
            chanceToReduce =
                ((metaGameConstants.maxFrustrationToConsiderReducingWaywardness - kid.frustration) / (metaGameConstants.maxFrustrationToConsiderReducingWaywardness))
                    * metaGameConstants.maxChanceOfReducingWaywardness
        in
            Random.map (\x -> x < chanceToReduce) (Random.float 0 1)
    else
        fixedGenerator False


reduceWaywardness : Model.Model -> Random.Generator (List Model.Kid)
reduceWaywardness model =
    let
        listOfChoicesGenerator =
            model.kids
                |> List.map shouldKidHaveWaywardnessReduced
                |> listOfGeneratorsToGeneratorOfList
    in
        Random.map (\boolList -> Utils.chooseByBoolList boolList model.kids) listOfChoicesGenerator


timeToWin : Int -> Random.Generator Float
timeToWin numKids =
    if numKids <= 5 then
        Random.float 30 60
    else
        Random.float 45 90

frustrationInit : Model.Kid -> Random.Generator Float
frustrationInit _ =
    Random.float 0 1

activityInit : Model.Kid -> Random.Generator Float
activityInit kid =
    Random.float 0 (min (gameConstants.highActivityThreshold - 0.1)  kid.waywardness)