module Tutorial exposing (view, numTutorialSteps)

import Msg
import ViewUtils
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


numTutorialSteps : Int
numTutorialSteps =
    3


view : Int -> Html Msg.Msg
view step =
    let
        ( msg, label ) =
            if step >= numTutorialSteps - 1 then
                ( Msg.StartMission, "Start!" )
            else
                ( Msg.TutorialNext, "Chápu..." )
    in
        div [ Attr.class "tutorial", Events.onClick (Msg.UI msg) ]
            [ img [ Attr.src ("img/tutorial/tutorial" ++ (toString (step + 1)) ++ ".png"), Attr.class "tutorialScreen" ] []
            , ViewUtils.viewButton (Msg.UI msg) "tutorialNext" label
            ]
