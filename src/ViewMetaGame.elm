module ViewMetaGame exposing (missionSummary, newGame)

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


viewNewlyAddedKid : Model.Kid -> Html Msg.Msg
viewNewlyAddedKid kid =
    td [ Attr.class "newKid" ]
        [ text (kid.name)
        , br [] []
        , text ("Zlobivost: " ++ toString (round (kid.waywardness * 10)) ++ "/10")
        ]


viewWaywardnessReducedKid : Model.Kid -> Html Msg.Msg
viewWaywardnessReducedKid kid =
    viewNewlyAddedKid kid


viewRemovedKid : Model.Kid -> Html Msg.Msg
viewRemovedKid kid =
    td [ Attr.class "removedKid" ]
        [ text (kid.name)
        , br [] []
        , text ("Zlobivost: " ++ toString (round (kid.waywardness * 10)) ++ "/10")
        ]


newGame : Model.Model -> Html Msg.Msg
newGame model =
    div [ Attr.class "newKidsContainer" ]
        [ text "Toto je tvůj oddíl:"
        , table [ Attr.class "changedKidsTable" ]
            [ tr [] (List.map viewNewlyAddedKid model.newlyAddedKids)
            ]
        ]


missionSummary : Model.Model -> Html Msg.Msg
missionSummary model =
    div [ Attr.class "missionSummary" ]
        [ div [ Attr.class "newKidsContainer" ]
            (if List.isEmpty model.newlyAddedKids then
                [ text
                    (case model.state of
                        Model.Lost _ ->
                            "Výprava se nevydařila, žádné nové děti nepřijdou."

                        _ ->
                            let
                                meanFrustration =
                                    model.kids |> List.map .frustration |> Utils.avg
                            in
                                "Žádné nové děti. Nové děti občas přijdou, když oddíl skončí výlet s dobrou celkovou náladou."
                    )
                ]
             else
                [ text "Děti měly dobrou náladu a přivedly do oddílu nováčky: "
                , table [ Attr.class "changedKidsTable" ]
                    [ tr [] (List.map viewNewlyAddedKid model.newlyAddedKids)
                    ]
                ]
            )
        , div [ Attr.class "reducedWaywardnessKidContainer" ]
            ((if List.isEmpty model.kidsWithReducedWaywardness then
                [ text
                    (case model.state of
                        Model.Lost _ ->
                            "Výprava se nevydařila, žádné děti se neuklidnily."

                        _ ->
                            "Žádné děti se neuklidnily. Děti, které skončí výpravu dobře naladěné se mohou uklidnit a příště již méně zlobit."
                    )
                ]
              else
                [ text "Tyto děti přijely domů dobře naladěné a příště budou méně zlobit: "
                , table [ Attr.class "changedKidsTable" ]
                    [ tr [] (List.map viewWaywardnessReducedKid model.kidsWithReducedWaywardness)
                    ]
                ]
             )
            )
        , div [ Attr.class "removedFrustratedKidsContainer" ]
            (if List.isEmpty model.removedFrustratedKids then
                []
             else
                [ text "Tyto děti měly z výletu špatnou náladu a odešly: "
                , table [ Attr.class "changedKidsTable" ]
                    [ tr [] (List.map viewRemovedKid model.removedFrustratedKids)
                    ]
                ]
            )
        , div [ Attr.class "removedKidsAfterMissionFailedContainer" ]
            (if List.isEmpty model.removedKidsAfterMissionFail then
                []
             else
                [ text "Některé děti mrzelo, že se výlet nevydařil a odešly: "
                , table [ Attr.class "changedKidsTable" ]
                    [ tr [] (List.map viewRemovedKid model.removedKidsAfterMissionFail)
                    ]
                ]
            )
        ]
