module ViewUtils
    exposing
        ( positionToStyle
        , horizontalProgress
        , verticalProgress
        , viewEmoji
        , viewEmojiSentence
        , viewButton
        , viewButtonEx
        , viewBasicUI
        , viewZoomButton
        , viewFrustrationSlider
        , viewStats
        )

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Msg
import Emojis
import GameConstants exposing (..)
import Model


positionToStyle : ( Int, Int ) -> List ( String, String )
positionToStyle ( x, y ) =
    [ ( "top", toString y ++ "px" ), ( "left", toString x ++ "px" ) ]


horizontalProgress : List (Attribute Msg.Msg) -> Float -> Html Msg.Msg
horizontalProgress attributes progress =
    div ([ Attr.class "horizontalProgressContainer" ] ++ attributes)
        [ div [ Attr.class "horizontalProgress", Attr.style [ ( "width", (toString ((progress * 100))) ++ "%" ) ] ]
            []
        ]


verticalProgress : List (Attribute Msg.Msg) -> Float -> Html Msg.Msg
verticalProgress attributes progress =
    div ([ Attr.class "verticalProgressContainer" ] ++ attributes)
        [ div [ Attr.class "verticalProgress", Attr.style [ ( "height", (toString ((progress * 100))) ++ "%" ) ] ]
            []
        ]


viewFrustrationSlider : Float -> Html Msg.Msg
viewFrustrationSlider frustration =
    div [ Attr.class "frustrationSlider" ]
        [ div [ Attr.class "sliderLine" ] []
        , div [ Attr.class "frustrationBar" ]
            --[ ViewUtils.horizontalProgress [] (1)
            [ div [ Attr.class "sliderMarker", Attr.style [ ( "left", toString (round ((frustration) * 100)) ++ "%" ) ] ] []
            ]
        , img [ Attr.class "frustrationHighIcon", Attr.src "img/ui/frustration_high_icon.png" ] []
        , img [ Attr.class "frustrationLowIcon", Attr.src "img/ui/frustration_low_icon.png" ] []
        ]


viewEmoji : Float -> String -> Html Msg.Msg
viewEmoji opacity emojiName =
    img
        [ Attr.class "emoji"
        , Attr.src ("img/emoji/" ++ emojiName ++ ".png")
        , Attr.style [ ( "opacity", toString opacity ) ]
        ]
        []


viewEmojiSentence : Float -> Emojis.Sentence -> List (Html Msg.Msg)
viewEmojiSentence coolDown sentence =
    let
        easeInTime =
            0.1

        easeInBorder =
            gameConstants.dialogCooldown - easeInTime

        opacity =
            if coolDown > easeInBorder then
                (1 - ((coolDown - easeInBorder) / easeInTime))
                    |> max 0
            else
                coolDown / easeInBorder
    in
        List.map (viewEmoji opacity) sentence


viewButtonEx : Bool -> Msg.Msg -> String -> String -> Html Msg.Msg
viewButtonEx disabled msg class label =
    a [ Attr.classList [ ( class, True ), ( "button", True ), ("disabled", disabled) ], Events.onClick msg ]
        [ text label
        ]


viewButton : Msg.Msg -> String -> String -> Html Msg.Msg
viewButton msg class label =
    viewButtonEx False msg class label


viewBasicUI : Model.Model -> { mainMessage : String, mainAction : Msg.UIMessage, mainActionTitle : String, mainMenuActionTitle : String, otherContents : List (Html Msg.Msg) } -> Html Msg.Msg
viewBasicUI model data =
    div [ Attr.class "basicUI" ]
        ([ div [ Attr.class "mainMessage" ] [ text data.mainMessage ]
         , viewButton (Msg.UI data.mainAction) "mainButton" data.mainActionTitle
         , viewButton (Msg.UI Msg.ShowMainMenu) "toMainMEnu" data.mainMenuActionTitle
         , viewZoomButton model
         , viewStats model
         ]
            ++ data.otherContents
        )


viewZoomButton : Model.Model -> Html Msg.Msg
viewZoomButton model =
    let
        ( msg, text ) =
            if model.scale == 1 then
                ( Msg.UI (Msg.SetScale 2), "Zvětšit" )
            else
                ( Msg.UI (Msg.SetScale 1), "Zmenšit" )
    in
        viewButton msg "zoomButton" text


viewStats : Model.Model -> Html Msg.Msg
viewStats model =
    div [ Attr.class "stats" ]
        [ table [ Attr.class "missionStats" ]
            [ tr []
                [ td [ Attr.colspan 2 ] [ text "Výpravy:" ]
                , td [ Attr.class "value" ] [ text (toString model.gameModel.numMissions) ]
                ]
            , tr [ Attr.class "positive" ]
                [ td [] [ text "-" ]
                , td [] [ text "Úspěšné" ]
                , td [ Attr.class "value" ] [ text (toString (model.gameModel.numMissions - model.gameModel.numFailures)) ]
                ]
            , tr [ Attr.class "negative" ]
                [ td [] [ text "-" ]
                , td [] [ text "Nevydařené" ]
                , td [ Attr.class "value" ] [ text (toString (model.gameModel.numFailures)) ]
                ]
            ]
        , table [ Attr.class "kidStats" ]
            [ tr []
                [ td [ Attr.colspan 2 ] [ text "Děti:" ]
                , td [ Attr.class "value" ] [ text (toString (List.length model.gameModel.kids)) ]
                ]
            , tr [ Attr.class "positive" ]
                [ td [] [ text "-" ]
                , td [] [ text "Vstoupily" ]
                , td [ Attr.class "value" ] [ text (toString (model.gameModel.numKidsAdded)) ]
                ]
            , tr [ Attr.class "negative" ]
                [ td [] [ text "-" ]
                , td [] [ text "Odešly" ]
                , td [ Attr.class "value" ] [ text (toString (model.gameModel.numKidsRemoved)) ]
                ]
            , tr [ Attr.class "positive" ]
                [ td [ Attr.colspan 2 ] [ text "Výchovné momenty:" ]
                , td [ Attr.class "value" ] [ text (toString model.gameModel.numKidsReducedWaywardness) ]
                ]
            ]
        ]
