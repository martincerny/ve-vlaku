module ViewGame exposing (view, viewKidGraphics)

import Model exposing (..)
import GameConstants exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Utils
import Emojis


kidPositions : List ( Int, Int )
kidPositions =
    let
        baseY =
            42
    in
        [ ( 24, baseY ), ( 72, baseY ), ( 152, baseY ), ( 200, baseY ) ]


windowPositions : List ( Int, Int )
windowPositions =
    [ ( 0, 0 ), ( 128, 0 ) ]


positionToStyle : ( Int, Int ) -> List ( String, String )
positionToStyle ( x, y ) =
    [ ( "top", toString y ++ "px" ), ( "left", toString x ++ "px" ) ]


valueToStyle : Float -> ( String, String )
valueToStyle value =
    ( "background-color", "rgb(" ++ (toString (144 + round (112 * value))) ++ ",200,200)" )


nervesToStyle : Float -> ( String, String )
nervesToStyle nerves =
    let
        threshold =
            gameConstants.nervesVisualChangeThreshold
    in
        ( "background-color"
        , if nerves < threshold then
            "white"
          else
            let
                scale =
                    (nerves - threshold) / (1 - threshold)

                otherColors =
                    toString (144 + round (112 * (1 - scale)))
            in
                "rgb(255," ++ otherColors ++ "," ++ otherColors ++ ")"
        )


horizontalProgress : List (Attribute Msg) -> Float -> Html Msg
horizontalProgress attributes progress =
    div ([ Attr.class "horizontalProgressContainer" ] ++ attributes)
        [ div [ Attr.class "horizontalProgress", Attr.style [ ( "width", (toString ((progress * 100))) ++ "%" ) ] ]
            []
        ]


viewEmoji : Float -> String -> Html Msg
viewEmoji opacity emojiName =
    img
        [ Attr.class "emoji"
        , Attr.src ("img/emoji/" ++ emojiName ++ ".png")
        , Attr.style [ ( "opacity", toString opacity ) ]
        ]
        []


viewEmojiSentence : Float -> Emojis.Sentence -> List (Html Msg)
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
        div [ Attr.classList [("kidGraphicsContainer", True), ("angry", angry) ] ]
            [ img [ Attr.class "armLeft", Attr.src ("img/kids/arms/" ++ g.arm ++ ".png") ] []
            , img [ Attr.class "body", Attr.src ("img/kids/body/" ++ g.body ++ ".png") ] []
            , img [ Attr.class "armRight", Attr.src ("img/kids/arms/" ++ g.arm ++ ".png") ] []
            , img [ Attr.class "scarf", Attr.src ("img/kids/scarf/" ++ g.scarf ++ ".png") ] []
            , img [ Attr.class "head", Attr.src ("img/kids/head/" ++ g.head ++ ".png") ] []
            , img [ Attr.class "eyes", Attr.src ("img/kids/eyes/" ++ eyes ++ ".png") ] []
            , img [ Attr.class "mouth", Attr.src ("img/kids/mouth/" ++ mouth ++ ".png") ] []
            , img [ Attr.class "hair", Attr.src ("img/kids/hair/" ++ g.hair ++ ".png") ] []
            ]


getKidPosition : PlayerActivity -> Int -> Kid -> ( Int, Int )
getKidPosition playerActivity positionId kid =
    let
        ( baseX, baseY ) =
            Utils.listGet positionId kidPositions
                |> Maybe.withDefault ( 300, 300 )
    in
        case playerActivity of
            CalmDownKid calmDownInfo ->
                if calmDownInfo.kidId == kid.id then
                    if positionId % 2 == 0 then
                        ( baseX + 16, baseY )
                    else
                        ( baseX - 16, baseY )
                else
                    ( baseX, baseY )

            _ ->
                ( baseX, baseY )


viewKid : PlayerActivity -> Int -> Kid -> ( String, Html Msg )
viewKid playerActivity positionId kid =
    let
        position =
            getKidPosition playerActivity positionId kid

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
            , Attr.style (positionToStyle position)
            , Events.onClick (Game (CalmDownStarted kid))
            ]
            [ viewKidGraphics angry mouthState kid.graphics
            ]
        )


viewKidUI : PlayerActivity -> Int -> Kid -> ( String, Html Msg )
viewKidUI playerActivity positionId kid =
    let
        position =
            getKidPosition playerActivity positionId kid
    in
        ( toString kid.id
        , div
            [ Attr.classList
                [ ( "kidUI", True )
                , ( "muted", isMuted kid )
                , ( "highActivity", isKidHighActivity kid )
                , ( "increasesNerves", isKidAnnoying kid )
                ]
            , Attr.style (positionToStyle position)
            , Events.onClick (Game (CalmDownStarted kid))
            ]
            [ viewKidDialog kid
            , div [ Attr.class "activityBar" ] [ horizontalProgress [] kid.activity ]
            , div [ Attr.class "frustrationBar" ] [ horizontalProgress [] (1 - kid.frustration) ]
              --            , tr [ Attr.class "small" ] [ td [] [ text ("Zlobivost: ") ], td [] [ text (toString (round (kid.waywardness * 10)) ++ "/10") ] ]
            , div [ Attr.class "kidName" ] [ text (kid.name) ]
            ]
        )


viewKidDialog : Kid -> Html Msg
viewKidDialog kid =
    (if kid.kidDialogCooldown > 0 then
        div [ Attr.class "kidDialog" ] (viewEmojiSentence kid.kidDialogCooldown kid.shownKidDialog)
     else
        text ""
    )


viewPlayerDialog : Kid -> Html Msg
viewPlayerDialog kid =
    td [ Attr.class "dialogCell" ]
        (if kid.playerDialogCooldown > 0 then
            [ div [ Attr.class "playerDialog" ] (viewEmojiSentence kid.playerDialogCooldown kid.shownPlayerDialog) ]
         else
            []
        )


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        ( position, flip ) =
            case model.playerActivity of
                CalmDownKid calmDownInfo ->
                    let
                        ( offsetX, shouldFlip ) =
                            if calmDownInfo.kidId % 2 == 0 then
                                ( 16, True )
                            else
                                ( 16, False )

                        ( baseX, baseY ) =
                            Utils.listGet calmDownInfo.kidId kidPositions
                                |> Maybe.withDefault ( 300, 300 )
                    in
                        ( ( baseX + offsetX, baseY - 16 ), shouldFlip )

                _ ->
                    ( ( 300, 300 ), False )
    in
        div
            [ Attr.classList [ ( "playerContainer", True ), ( "flip", flip ) ]
            , Attr.style (positionToStyle position)
            ]
            [ img [ Attr.class "legLeft", Attr.src "img/leader/leg_leader.png" ] []
            , img [ Attr.class "legRight", Attr.src "img/leader/leg_leader.png" ] []
            , img [ Attr.class "armLeft", Attr.src "img/leader/arm_leader.png" ] []
            , img [ Attr.class "body", Attr.src "img/leader/body_leader.png" ] []
            , img [ Attr.class "armRight", Attr.src "img/leader/arm_leader.png" ] []
            , img [ Attr.class "scarf", Attr.src "img/leader/scarf_leader.png" ] []
            , img [ Attr.class "head", Attr.src "img/leader/head_leader.png" ] []
            , img [ Attr.class "hat", Attr.src "img/leader/hat_leader.png" ] []
            ]


viewWindow : ( Int, Int ) -> Html Msg
viewWindow position =
    img [ Attr.class "trainWindow", Attr.style (positionToStyle position), Attr.src "img/train/window.png" ] []


view : Model -> Html Msg
view model =
    div
        [ Attr.style [ nervesToStyle model.nerves ]
        ]
        [ Keyed.node "div"
            [ Attr.class "allKidsContainer" ]
            (( "player", viewPlayer model )
                :: (List.indexedMap (viewKid model.playerActivity) model.kids)
            )
        , div [ Attr.class "train" ]
            (List.map viewWindow windowPositions)
        , Keyed.node "div"
            [ Attr.class "allKidsUIContainer" ]
            (List.indexedMap (viewKidUI model.playerActivity) model.kids)
        , table []
            [ tr [] (List.map viewKidDialog model.kids)
            , tr [] (List.map viewPlayerDialog model.kids)
            ]
        , div
            [ Attr.classList
                [ ( "takeDeepBreath", True )
                , ( "active", model.playerActivity == DeepBreath )
                , ( "highlighted", not (model.playerActivity == DeepBreath) && model.nerves > 0.9 )
                ]
            , Events.onMouseDown (Game DeepBreathStarted)
            , Events.onMouseUp (Game DeepBreathEnded)
            , Events.onMouseOut (Game DeepBreathEnded)
            ]
            [ text ("Zhluboka dýchej")
            ]
        , (case model.playerActivity of
            CalmDownKid _ ->
                div [ Attr.class "noPlayer" ] []

            DeepBreath ->
                div [ Attr.class "player" ] [ text "(tu vydechuješ)" ]

            None ->
                div [ Attr.class "player" ] [ text "(tu stojíš)" ]
          )
        , table []
            [ tr []
                [ td []
                    [ div [ Attr.class "nervesSliderContainer" ]
                        [ div
                            [ Attr.class "nervesSlider"
                            , Attr.style [ ( "bottom", (toString ((model.nerves * 100))) ++ "%" ) ]
                            ]
                            []
                        ]
                    ]
                , td []
                    [ div [ Attr.class "nervesSliderContainer" ]
                        [ div
                            [ Attr.class "nervesSlider"
                            , Attr.style [ ( "bottom", (toString ((model.highActivityScore * 100) / gameConstants.highActivityScoreToLose)) ++ "%" ) ]
                            ]
                            []
                        ]
                    ]
                , td [] [ text ("Čas do cílové stanice") ]
                , td [] [ text ("Celková nálada") ]
                ]
            , tr []
                [ td [] [ text ("Tvoje nervy") ]
                , td [] [ text ("Nervy průvodčího") ]
                , td []
                    [ text
                        (toString ((round model.timeToWin) // 60)
                            ++ ":"
                            ++ Utils.fixedWidthNumberFormat 2 ((round model.timeToWin) % 60)
                        )
                    ]
                , td [] [ horizontalProgress [] (1 - (model.kids |> List.map .frustration |> Utils.avg)) ]
                ]
            ]
        ]
