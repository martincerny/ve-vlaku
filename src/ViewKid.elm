module ViewKid exposing (viewKid, viewKidGraphics, viewKidUI, viewKidWaywardness)

import Model exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import GameConstants exposing (..)
import ViewUtils
import Emojis


viewKidGraphics : Bool -> Model.KidMouthState -> Model.KidGraphics -> Html a
viewKidGraphics angry mouthState g =
    let
        eyes =
            if angry then
                g.eyesAngry
            else
                g.eyes

        mouth =
            case mouthState of
                Happy ->
                    g.mouthHappy

                Sad ->
                    g.mouthSad

                Neutral ->
                    g.mouthNeutral
    in
        div [ Attr.classList [ ( "kidGraphicsContainer", True ), ( "angry", angry ) ] ]
            [ img [ Attr.class "armLeft", Attr.src ("img/kids/arms/" ++ g.arm ++ ".png") ] []
            , img [ Attr.class "body", Attr.src ("img/kids/body/" ++ g.body ++ ".png") ] []
            , img [ Attr.class "armRight", Attr.src ("img/kids/arms/" ++ g.arm ++ ".png") ] []
            , img [ Attr.class "scarf", Attr.src ("img/kids/scarf/" ++ g.scarf ++ ".png") ] []
            , img [ Attr.class "head", Attr.src ("img/kids/head/" ++ g.head ++ ".png") ] []
            , img [ Attr.class "eyes", Attr.src ("img/kids/eyes/" ++ eyes ++ ".png") ] []
            , img [ Attr.class "mouth", Attr.src ("img/kids/mouth/" ++ mouth ++ ".png") ] []
            , img [ Attr.class "hair", Attr.src ("img/kids/hair/" ++ g.hair ++ ".png") ] []
            ]


viewKidWaywardness : Kid -> Html Msg
viewKidWaywardness kid =
    let
        numIcons =
            round (kid.waywardness / 0.2)
    in
        div [ Attr.class "waywardness" ]
            (List.map
                (\id ->
                    img
                        [ Attr.src "img/ui/waywardness_icon2.png"
                        , Attr.class "waywardnessIcon"
                        , Attr.style (ViewUtils.positionToStyle ( id * 10, 0 ))
                        ]
                        []
                )
                [1..numIcons]
            )


viewKid : PlayerActivity -> ( Int, Int ) -> Kid -> ( String, Html Msg )
viewKid playerActivity position kid =
    let
        angry =
            if isMuted kid then
                False
            else if (kid.timeSinceLastOutburst < 0.5 && kid.kidDialogCooldown > 0) then
                True
            else if (kid.scheduledOutburst.interval - kid.timeSinceLastOutburst) < 0.5 then
                True
            else
                False

        mouthState =
            if kid.frustration > metaGameConstants.minFrustrationToConsiderRemovingKid then
                Model.Sad
            else if kid.frustration < metaGameConstants.maxFrustrationToConsiderReducingWaywardness then
                Model.Happy
            else
                Model.Neutral
    in
        ( toString kid.id
        , div
            [ Attr.classList
                [ ( "kid", True )
                , ( "muted", isMuted kid )
                , ( "highActivity", isKidHighActivity kid )
                , ( "increasesNerves", isKidAnnoying kid )
                ]
            , Attr.style (ViewUtils.positionToStyle position)
            , Events.onClick (Game (CalmDownStarted kid))
            ]
            [ viewKidGraphics angry mouthState kid.graphics
            ]
        )


viewKidDialog : Kid -> Html Msg
viewKidDialog kid =
    (if kid.kidDialogCooldown > 0 then
        div [ Attr.class "kidDialog" ] (ViewUtils.viewEmojiSentence kid.kidDialogCooldown kid.shownKidDialog)
     else
        text ""
    )


viewPlayerDialog : Kid -> Html Msg
viewPlayerDialog kid =
    (if kid.playerDialogCooldown > 0 then
        div [ Attr.classList [ ( "playerDialog", True ), ( "left", kid.positionId % 2 == 0 ) ] ] (ViewUtils.viewEmojiSentence kid.playerDialogCooldown kid.shownPlayerDialog)
     else
        text ""
    )


viewKidUI : PlayerActivity -> ( Int, Int ) -> Kid -> ( String, Html Msg )
viewKidUI playerActivity position kid =
    ( toString kid.id
    , div
        [ Attr.classList
            [ ( "kidUI", True )
            , ( "muted", isMuted kid )
            , ( "highActivity", isKidHighActivity kid )
            , ( "annoying", isKidAnnoying kid && not (isKidHighActivity kid) )
            , ( "sad", kid.frustration > metaGameConstants.minFrustrationToConsiderRemovingKid )
            , ( "happy", kid.frustration < metaGameConstants.maxFrustrationToConsiderReducingWaywardness )
            ]
        , Attr.style (ViewUtils.positionToStyle position)
        , Events.onClick (Game (CalmDownStarted kid))
        ]
        [ viewKidDialog kid
        , div [ Attr.class "activityBar" ] [ ViewUtils.horizontalProgress [] kid.activity ]
        , img [ Attr.class "activityIcon", Attr.src "img/ui/activity_icon.png" ] []
        , div [ Attr.class "frustrationBar" ] [ ViewUtils.horizontalProgress [] (1 - kid.frustration) ]
        , img [ Attr.class "frustrationIcon", Attr.src "img/ui/frustration_icon.png" ] []
        , viewKidWaywardness kid
        , div [ Attr.class "kidName" ] [ text (kid.name) ]
        , viewKidDialog kid
        , viewPlayerDialog kid
        ]
    )
