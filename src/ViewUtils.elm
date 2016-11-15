module ViewUtils exposing (positionToStyle, horizontalProgress, verticalProgress, viewEmoji, 
    viewEmojiSentence
    ,viewBasicUI)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Msg
import Emojis
import GameConstants exposing(..)

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

viewBasicUI : {mainMessage : String, mainAction : Msg.UIMessage, mainActionTitle : String, mainMenuActionTitle : String} -> Html Msg.Msg
viewBasicUI data =
    div [] 
    [ div [Attr.class "mainMessage"] [text data.mainMessage]
    , div [Attr.class "mainButton", Events.onClick (Msg.UI data.mainAction)] [text data.mainActionTitle]
    , div [Attr.class "toMainMenu", Events.onClick (Msg.UI Msg.ShowMainMenu)] [ text data.mainMenuActionTitle]
    ]

