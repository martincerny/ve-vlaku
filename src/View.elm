module View exposing (view)

import Model
import GameConstants exposing (..)
import Msg
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Utils
import Emojis
import ViewGame
import ViewMetaGame
import ViewUtils
import Preload


viewMainMenu : Model.Model -> Html Msg.Msg
viewMainMenu model =
    div [ Attr.class "mainMenu" ]
        [ div [ Attr.class "newGame", Events.onClick (Msg.UI Msg.StartNewGame) ]
            [ text "Nová hra"
            ]
        ]


viewPausedGame : Model.Model -> List (Html Msg.Msg)
viewPausedGame model =
    [ ViewGame.view model.gameModel
    , div
        [ Attr.classList
            [ ( "gameOverlay", True )
            , ( "PauseMission", True )
            ]
        ]
        [ ViewUtils.viewBasicUI
            { mainMessage = "Pauznuto"
            , mainAction = Msg.ResumeMission
            , mainActionTitle = "Pokračovat"
            , mainMenuActionTitle = "Ukončit misi"
            }
        ]
    ]


view : Model.Model -> Html Msg.Msg
view model =
    div
        [ Attr.class "topContainer", Attr.style [ ( "transform", "scale(" ++ (toString model.scale) ++ ")" ) ] ]
        ((case model.uiState of
            Model.MainMenu ->
                [ viewMainMenu model ]

            Model.BeforeMission ->
                [ ViewMetaGame.beforeMission model ]

            Model.RunningGame ->
                [ ViewGame.view model.gameModel ]

            Model.PausedGame ->
                viewPausedGame model

            Model.MissionSummary ->
                [ ViewMetaGame.missionSummary model ]
         )
            ++ [ div [ Attr.class "zoomButton" ]
                    [ if model.scale == 1 then
                        a [ Events.onClick (Msg.UI (Msg.SetScale 2)) ] [ text "Zvětšit" ]
                      else
                        a [ Events.onClick (Msg.UI (Msg.SetScale 1)) ] [ text "Zmenšit" ]
                    ]
               , div [ Attr.style [ ( "display", "none" ) ] ]
                    (List.map (\x -> img [ Attr.src x ] []) Preload.images)
               ]
        )
