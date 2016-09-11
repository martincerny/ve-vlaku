module View exposing (view)

import Model exposing(..)
import GameConstants exposing(..)
import Msg exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events

valueToStyle : Float -> (String, String)
valueToStyle value =
  ("background-color", "rgb(" ++ (toString (144 + round (112 * value))) ++ ",200,200)")

nervesToStyle : Float -> (String, String) 
nervesToStyle nerves =
  let 
    threshold = 
      gameConstants.nervesVisualChangeThreshold    
  in
    ("background-color", 
      if nerves < threshold then "white"
      else
        let 
          scale =  
            (nerves - threshold) / (1 - threshold)            
          otherColors = 
            toString (144 + round(112 * (1 - scale) ))
        in
          "rgb(255," ++ otherColors ++ "," ++ otherColors ++")"  
    )

viewKid : Kid -> Html Msg
viewKid kid =
  td 
    [ 
      Attr.style  (if isMuted kid then [] else [valueToStyle kid.activity])
      , Attr.classList [
        ("kid", True)
        , ("muted", isMuted kid)
        , ("highActivity", isKidHighActivity kid)
        , ("increasesNerves", isKidIncreasingNerves kid)
        ]
      , Events.onClick (CalmDown kid)
    ] [
      div [] [text (kid.name)] 
      , div [] [text (toString (round (100 * kid.activity)))]
    ]

view : Model -> Html Msg
view model =
  div [
     Attr.style [ nervesToStyle model.nerves ]    
  ] [
    table [] [ tr [] 
      (List.map viewKid model.kids)         
    ]    
    , div
       ( 
       [Attr.classList [
          ("gameOverlay", True)
          , ("disableGame", model.lost || model.paused)
       ]]                
        ++  if model.paused then [Events.onClick ResumeGame]
            else if model.lost then [Events.onClick RestartGame]
            else []
       )
      [
        if model.lost then text("Průvodčí tě vyhodil") 
        else if model.paused then text("Klikni pro spuštění")
        else text("")
      ]  
    , div [ 
        Attr.classList [
            ("takeDeepBreath", True)
            , ("active", model.takingDeepBreath)
            , ("highlighted", not model.takingDeepBreath && model.nerves > 1 - gameConstants.calmDownNervesGrowth)
            ]
        , Events.onMouseDown DeepBreathStarted 
        , Events.onMouseUp DeepBreathEnded
        , Events.onMouseOut DeepBreathEnded
      ] [
        text ("Zhluboka dýchej")
      ]
      , 
      table [] [
        tr [] [
          td [] [
            div [Attr.class "nervesSliderContainer"]
            [
              div [
                Attr.class "nervesSlider"
                , Attr.style [("bottom", (toString ((model.nerves * 100))) ++ "%")]
              ] []
            ]      
          ]
          , td [] [
            div [Attr.class "nervesSliderContainer"]
            [
              div [
                Attr.class "nervesSlider"
                , Attr.style [("bottom", (toString ((model.highActivityScore * 100) / gameConstants.highActivityScoreToLose)) ++ "%")]
              ] []
            ]      
          ]
        ]
        , tr [] [
          td [] [text("Tvoje nervy")]
          , td [] [text("Nervy průvodčího")]
        ]
      ]
  ]