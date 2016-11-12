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


viewKid : PlayerActivity -> Kid -> ( String, Html Msg )
viewKid playerActivity kid =
    ( toString kid.id
    , td
        [ Attr.style
            (if isMuted kid then
                []
             else
                [ valueToStyle kid.activity ]
            )
        , Attr.classList
            [ ( "kid", True )
            , ( "muted", isMuted kid )
            , ( "highActivity", isKidHighActivity kid )
            , ( "increasesNerves", isKidAnnoying kid )
            ]
        , Events.onClick (Game (CalmDownStarted kid))
        ]
        [ table []
            [ tr [] [ td [] [ text ("Rozjetost: ") ], td [] [ horizontalProgress [] kid.activity ] ]
            , tr [] [ td [] [ text ("Nálada: ") ], td [] [ horizontalProgress [] (1 - kid.frustration) ] ]
            , tr [ Attr.class "small" ] [ td [] [ text ("Zlobivost: ") ], td [] [ text (toString (round (kid.waywardness * 10)) ++ "/10") ] ]
            , tr [ Attr.class "small" ] [ td [] [ text ("Kliků: ") ], td [] [ text (toString kid.numCalmDowns) ] ]
            , tr [ Attr.class "small" ]
                [ td [] [ text ("Avg int.: ") ]
                , td [] [ text (toString (kid.sumOutburstIntervals / (toFloat kid.numScheduledOutbursts))) ]
                ]
            ]
        , div [] [ text (kid.name) ]
        ]
    )


viewKidDialog : Kid -> Html Msg
viewKidDialog kid =
    td [ Attr.class "dialogCell" ]
        (if kid.kidDialogCooldown > 0 then
            [ div [ Attr.class "kidDialog" ] (viewEmojiSentence kid.kidDialogCooldown kid.shownKidDialog) ]
         else
            []
        )


viewPlayerDialog : Kid -> Html Msg
viewPlayerDialog kid =
    td [ Attr.class "dialogCell" ]
        (if kid.playerDialogCooldown > 0 then
            [ div [ Attr.class "playerDialog" ] (viewEmojiSentence kid.playerDialogCooldown kid.shownPlayerDialog) ]
         else
            []
        )


viewPlayerNextToKid : Model -> Kid -> Html Msg
viewPlayerNextToKid model kid =
    let
        playerHere =
            case model.playerActivity of
                CalmDownKid calmDownInfo ->
                    calmDownInfo.kidId == kid.id

                _ ->
                    False
    in
        if playerHere then
            td [ Attr.class "playerAtKid" ] [ text ("(tu sedíš)") ]
        else
            td [] []


viewNewlyAddedKid : Kid -> Html Msg
viewNewlyAddedKid kid =
    td [ Attr.class "newKid" ]
        [ text (kid.name)
        , br [] []
        , text ("Zlobivost: " ++ toString (round (kid.waywardness * 10)) ++ "/10")
        ]


view : Model -> Html Msg
view model =
    div
        [ Attr.style [ nervesToStyle model.nerves ]
        ]
        [ table []
            [ Keyed.node "tr" [] (List.map (viewKid model.playerActivity) model.kids)
            , tr [] (List.map viewKidDialog model.kids)
            , tr [] (List.map viewPlayerDialog model.kids)
            , tr [ Attr.class "playerRow" ] (List.map (viewPlayerNextToKid model) model.kids)
            ]
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
                    case reason of
                        Activity ->
                            [ text ("Průvodčí vás vyhodil z vlaku :-(") ]

                        Nerves ->
                            [ text ("Ruply ti nervy :-(") ]

                Won ->
                    [ text ("Dojeli jste na místo. Hurá!") ]

                Running ->
                    []
             )
                ++ (if List.isEmpty model.newlyAddedKids then
                        []
                    else
                        [ div [ Attr.class "newKidsContainer" ]
                            [ text
                                (if model.firstRun then
                                    "Tohle je tvůj oddíl:"
                                 else
                                    "Děti měly dobrou náladu a přivedly do oddílu nováčky: "
                                )
                            , table [ Attr.class "newKidsTable" ]
                                [ tr [] (List.map viewNewlyAddedKid model.newlyAddedKids)
                                ]
                            ]
                        ]
                   )
            )
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
                ]
            ]
        ]
