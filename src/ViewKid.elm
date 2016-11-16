module ViewKid exposing (viewKid, viewKidGraphics, viewKidUI, viewKidWaywardness)

import Model
import Msg
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
                Model.Happy ->
                    g.mouthHappy

                Model.Sad ->
                    g.mouthSad

                Model.Neutral ->
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


viewKidWaywardness : Model.Kid -> Html Msg.Msg
viewKidWaywardness kid =
    let
        numIcons =
            round (kid.waywardness / 0.2)
    in
        div [ Attr.class "waywardness" ]
            (List.map
                (\id ->
                    img
                        [ Attr.src "img/ui/waywardness_icon.png"
                        , Attr.class "waywardnessIcon"
                        , Attr.style (ViewUtils.positionToStyle ( id * 10, 0 ))
                        ]
                        []
                )
                [1..numIcons]
            )


viewKid : Model.PlayerActivity -> ( Int, Int ) -> Model.Kid -> ( String, Html Msg.Msg )
viewKid playerActivity position kid =
    let
        angry =
            if Model.isMuted kid then
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
                , ( "muted", Model.isMuted kid )
                , ( "highActivity", Model.isKidHighActivity kid )
                , ( "increasesNerves", Model.isKidAnnoying kid )
                ]
            , Attr.style (ViewUtils.positionToStyle position)
            , Events.onClick (Msg.Game (Msg.CalmDownStarted kid))
            ]
            [ viewKidGraphics angry mouthState kid.graphics
            ]
        )


viewKidDialog : Model.Kid -> Html Msg.Msg
viewKidDialog kid =
    (if kid.kidDialogCooldown > 0 then
        div [ Attr.class "kidDialog" ] (ViewUtils.viewEmojiSentence kid.kidDialogCooldown kid.shownKidDialog)
     else
        text ""
    )


viewPlayerDialog : Model.Kid -> Html Msg.Msg
viewPlayerDialog kid =
    (if kid.playerDialogCooldown > 0 then
        div [ Attr.classList [ ( "playerDialog", True ), ( "left", kid.positionId % 2 == 0 ) ] ] (ViewUtils.viewEmojiSentence kid.playerDialogCooldown kid.shownPlayerDialog)
     else
        text ""
    )


viewVolume : Float -> Html Msg.Msg
viewVolume volume =
    div [ Attr.class "volume" ]
        [ img [ Attr.class "background", Attr.src "img/ui/volume_background.png" ] []
        , div [ Attr.class "foreground", Attr.style [ ( "width", toString (round (volume * 100)) ++ "%" ) ] ]
            [ img [ Attr.src "img/ui/volume_foreground.png" ] []
            ]
        ]


viewFrustrationSlider : Model.Kid -> Html Msg.Msg
viewFrustrationSlider kid =
    div [ Attr.class "frustrationSlider" ]
        [ div [ Attr.class "sliderLine" ] []
        , div [ Attr.class "frustrationBar" ]
            --[ ViewUtils.horizontalProgress [] (1)
            [ div [ Attr.class "sliderMarker", Attr.style [ ( "left", toString (round ((kid.frustration) * 100)) ++ "%" ) ] ] []
            ]
        , img [ Attr.class "frustrationHighIcon", Attr.src "img/ui/frustration_high_icon.png" ] []
        , img [ Attr.class "frustrationLowIcon", Attr.src "img/ui/frustration_low_icon.png" ] []
        ]


viewKidUI : Model.PlayerActivity -> ( Int, Int ) -> Model.Kid -> ( String, Html Msg.Msg )
viewKidUI playerActivity position kid =
    ( toString kid.id
    , div
        [ Attr.classList
            [ ( "kidUI", True )
            , ( "muted", Model.isMuted kid )
            , ( "highActivity", Model.isKidHighActivity kid )
            , ( "annoying", Model.isKidAnnoying kid && not (Model.isKidHighActivity kid) )
            , ( "sad", kid.frustration > metaGameConstants.minFrustrationToConsiderRemovingKid )
            , ( "happy", kid.frustration < metaGameConstants.maxFrustrationToConsiderReducingWaywardness )
            ]
        , Attr.style (ViewUtils.positionToStyle position)
        , Events.onClick (Msg.Game (Msg.CalmDownStarted kid))
        ]
        [ viewKidDialog kid
          --        , viewVolume kid.activity
        , div [ Attr.class "activityBar" ] [ ViewUtils.horizontalProgress [] kid.activity ]
        , img [ Attr.class "activityIcon", Attr.src "img/ui/activity_icon.png" ] []
        , viewFrustrationSlider kid
        , viewKidWaywardness kid
        , div [ Attr.class "kidName" ] [ text (kid.name) ]
        , viewKidDialog kid
        , viewPlayerDialog kid
        ]
    )
