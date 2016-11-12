module View exposing (view)

import Model exposing (..)
import GameConstants exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Utils
import Emojis
import ViewGame
import ViewMetaGame


view : Model -> Html Msg
view model =
    div
        []
        [ ViewGame.view model
        , div
            ([ Attr.classList
                [ ( "gameOverlay", True )
                , ( "disableGame", not (shouldUpdateGame model) )
                ]
             ]
                ++ case model.state of
                    Paused ->
                        [ Events.onClick (UI ResumeGame) ]

                    Lost _ ->
                        [ Events.onClick (UI ResumeGame) ]

                    Won ->
                        [ Events.onClick (UI ResumeGame) ]

                    Running ->
                        []
            )
            ((case model.state of
                Paused ->
                    [ text ("Klikni pro spuštění") ]

                Lost reason ->
                    let 
                        lostMessage =
                            case reason of
                                Activity ->
                                    "Průvodčí vás vyhodil z vlaku :-("

                                Nerves ->
                                    "Ruply ti nervy :-("
                    in 
                    [ text lostMessage
                    , ViewMetaGame.missionSummary model
                    ]

                Won ->
                    [ text ("Dojeli jste na místo. Hurá!")
                    , ViewMetaGame.missionSummary model 
                    ]

                Running ->
                    []
             )

            )
        ]
