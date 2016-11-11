module RandomGenerators
    exposing
        ( outburstParams
        , addKidAfterWin
        , timeToWin
        , frustrationRecoveryInterval
        )

import GameConstants exposing (..)
import Random
import Model
import KidGenerator


fixedGenerator : a -> Random.Generator a
fixedGenerator value =
    Random.map (\_ -> value) Random.bool



--I do not consume the bool, but there is no way to create my own primitive generator


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
            (model.kids |> List.map .frustration |> List.sum) / (toFloat (List.length model.kids))
    in
        if meanFrustration < gameConstants.maximalMeanFrustrationToAddKid then
            let
                chance =
                    (1 - (meanFrustration / gameConstants.maximalMeanFrustrationToAddKid))
                        * gameConstants.maximalChanceOfAddingKid
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


timeToWin : Int -> Random.Generator Float
timeToWin numKids =
    if numKids < 4 then
        Random.float 30 60
    else
        Random.float 45 90
