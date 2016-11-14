module ViewGame exposing (view)

import Model exposing (..)
import GameConstants exposing (..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Utils
import Emojis
import ViewUtils
import ViewKid


windowPositions : List ( Int, Int )
windowPositions =
    List.map (\x -> ( x * 128, 0 )) [0..3]
        ++ List.map (\x -> ( x * 128, 128 )) [0..3]


kidPositions : List ( Int, Int )
kidPositions =
    let
        positionPairs =
            List.map (\( x, y ) -> ( ( x + 24, y + 42 ), ( x + 72, y + 42 ) )) windowPositions
    in
        List.foldr (\( pairA, pairB ) rest -> pairA :: pairB :: rest) [] positionPairs




getKidPosition : PlayerActivity -> Kid -> ( Int, Int )
getKidPosition playerActivity kid =
    let
        ( baseX, baseY ) =
            Utils.listGet kid.positionId kidPositions
                |> Maybe.withDefault ( 300, 300 )
    in
        case playerActivity of
            CalmDownKid calmDownInfo ->
                if calmDownInfo.positionId == kid.positionId then
                    if kid.positionId % 2 == 0 then
                        ( baseX + 16, baseY )
                    else
                        ( baseX - 16, baseY )
                else if kid.positionId % 2 == 0 && calmDownInfo.positionId == kid.positionId + 1 then
                        ( baseX - 8, baseY )
                else if kid.positionId % 2 == 1 && calmDownInfo.positionId == kid.positionId - 1 then
                        ( baseX + 8, baseY )
                else
                    ( baseX, baseY )

            _ ->
                ( baseX, baseY )



playerPositionForCalmDown : Model.CalmDownInfo -> { position : ( Int, Int ), flip : Bool }
playerPositionForCalmDown calmDownInfo =
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
        { position = ( baseX + offsetX, baseY - 16 ), flip = shouldFlip }


viewPlayer : { position : ( Int, Int ), flip : Bool } -> List String -> Html Msg
viewPlayer data additionalClasses =
    let
        ( position, flip ) =
            ( data.position, data.flip )

        classList =
            [ ( "playerContainer", True ), ( "flip", flip ) ]
                ++ (List.map (\x -> ( x, True )) additionalClasses)
    in
        div
            [ Attr.classList classList
            , Attr.style (ViewUtils.positionToStyle position)
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
    img [ Attr.class "trainWindow", Attr.style (ViewUtils.positionToStyle position), Attr.src "img/train/window.png" ] []


view : Model -> Html Msg
view model =
    div
        [ ]
        [ Keyed.node "div"
            [ Attr.class "allKidsContainer" ]
            ((case model.playerActivity of
                CalmDownKid calmDownInfo ->
                    ( "player", viewPlayer (playerPositionForCalmDown calmDownInfo) [] )

                _ ->
                    ( "noPlayer", text ("") )
             )
                :: (List.map (\kid -> ViewKid.viewKid model.playerActivity (getKidPosition model.playerActivity kid) kid) model.kids)
            )
        , div [ Attr.class "train" ]
            (div [ Attr.class "filler" ] [] :: (List.map viewWindow windowPositions))
        , Keyed.node "div"
            [ Attr.class "allKidsUIContainer" ]
            (List.map (\kid -> ViewKid.viewKidUI model.playerActivity (getKidPosition model.playerActivity kid) kid) model.kids)        
        , div
            [ Attr.classList
                [ ( "takeDeepBreath", True )
                , ( "active", model.playerActivity == DeepBreath )
                , ( "highlighted", not (model.playerActivity == DeepBreath) && model.nerves > 0.9 && (round (model.timeToWin * 4) % 2 == 0) )
                ]
            , Events.onMouseDown (Game DeepBreathStarted)
            , Events.onMouseUp (Game DeepBreathEnded)
            , Events.onMouseOut (Game DeepBreathEnded)
            ]
            [ text ("Zhluboka dýchej")
            , div [ Attr.class "small" ] [ text "(Klikni sem a drž)" ]
            , (case model.playerActivity of
                CalmDownKid _ ->
                    text ("")

                _ ->
                    div [ Attr.class "playerDeepBreath" ] [ viewPlayer { position = ( 0, 0 ), flip = False } [] ]
              )
            ]
        , div [ Attr.classList [ ( "playerNerves", True ), ( "highlight", model.nerves > 0.8 ) ] ] [ ViewUtils.verticalProgress [] model.nerves ]
        , div [ Attr.class "playerNervesLabel" ] [ text "Tvoje nervy" ]
        , div [ Attr.classList [ ( "highActivityScore", True ), ( "highlight", model.highActivityScore > 0.8 ) ] ]
            [ ViewUtils.verticalProgress [] model.highActivityScore ]
        , div [ Attr.class "highActivityScoreLabel" ] [ text "Nervy průvodčího" ]
        , div [ Attr.class "timeToWin" ]
            [ table []
                [ tr []
                    [ td [] [ text ("Čas do cílové stanice") ]
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
        , div [ Attr.class "overallFrustration" ]
            [ ViewUtils.horizontalProgress [] (1 - (model.kids |> List.map .frustration |> Utils.avg))
            , img [ Attr.class "frustrationIcon", Attr.src "img/ui/frustration_icon.png" ] []
            ]
        , div [ Attr.class "overallFrustrationLabel" ] [ text "Celková nálada" ]
        ]
