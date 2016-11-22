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
        [ div [Attr.class "gameTitle"] [text "Ve vlaku"]
        , div [Attr.class "gameSubtitle"] [text "Krotíme děti pro trochu lepší svět"]
        , ViewUtils.viewButton (Msg.UI Msg.StartNewGame) "newGame" "Nová hra"
        , ViewUtils.viewButtonEx (model.gameModel.numMissions <= 0) (Msg.UI Msg.StartMission) "continueGame" "Pokračovat"
        , ViewUtils.viewZoomButton model
        , if model.gameModel.numMissions > 0 then
            ViewUtils.viewStats model
          else
            text ""
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
        [ ViewUtils.viewBasicUI model
            { mainMessage = "Pauznuto"
            , mainAction = Msg.ResumeMission
            , mainActionTitle = "Pokračovat"
            , mainMenuActionTitle = "Ukončit misi"
            , otherContents = []
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
            ++ [ div [ Attr.style [ ( "display", "none" ) ] ]
                    (List.map (\x -> img [ Attr.src x ] []) Preload.images)
               ]
        )
