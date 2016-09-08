module View exposing (view)

import Model exposing(..)
import Msg exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events

valueToStyle : Float -> (String, String)
valueToStyle value =
  ("background-color", "rgb(" ++ (toString (144 + round (112 * value))) ++ ",200,200)")

viewKid : Kid -> Html Msg
viewKid kid =
  td 
    [ 
      Attr.style  [valueToStyle kid.activity]
      , Attr.class "kid"
    ] [
      div [] [text (kid.name)] 
      , div [] [text (toString (round (100 * kid.activity)))]
    ]

view : Model -> Html Msg
view model =
  div [] [
    table [] [ tr [] 
      (List.map viewKid model.kids)         
    ]
    , div [
        Attr.style [valueToStyle(model.nerves)]
        , Attr.class "nervesDisplay"
      ] [
        text (toString (round (100 * model.nerves))) 
      ]
    , div [ 
        Attr.classList [
            ("takeDeepBreath", True)
            , ("active", model.takingDeepBreath)
            ]
        , Events.onMouseDown DeepBreathStarted 
        , Events.onMouseUp DeepBreathEnded
        , Events.onMouseOut DeepBreathEnded
      ] [
        text ("Zhluboka dýchej")
      ]
      , div []
        ( 
        (if model.lost then text("Lost ") else text(""))
        :: (if isHighActivity model then text(" High activity ") else  text(""))
        :: []
        )
  ]