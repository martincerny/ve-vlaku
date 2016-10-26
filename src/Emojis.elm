module Emojis
    exposing
        ( Sentence
        , nothing
        , calmDown
        , frustrationRecovery
        , outburst
        )

import GameConstants exposing (..)


type alias Sentence =
    List String


nothing : Sentence
nothing =
    []


calmDown : Float -> Sentence
calmDown kidActivity =
    if kidActivity >= gameConstants.highActivityThreshold then
        [ "angry2", "hush" ]
    else if kidActivity >= gameConstants.annoyingActivityThreshold then
        [ "hush" ]
    else
        [ "thumbsUp" ]


frustrationRecovery : Float -> Sentence
frustrationRecovery kidFrustration =
    if kidFrustration < 0.3 then
        [ "thumbsUp" ]
    else if kidFrustration < 0.7 then
        [ "smile" ]
    else
        [ "friends" ]


outburst : Float -> Sentence
outburst intensity =
    if intensity < 0.3 then
        [ "laugh" ]
    else if intensity < 0.7 then
        [ "joking", "exclamation" ]
    else
        [ "angry", "fist" ]
