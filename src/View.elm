module View exposing (view)

import Model exposing(..)
import GameConstants exposing(..)
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Utils
import Texts

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

horizontalProgress : List (Attribute Msg) -> Float -> Html Msg
horizontalProgress attributes progress =
  div ([ Attr.class "horizontalProgressContainer" ] ++ attributes)
  [ 
    div [ Attr.class "horizontalProgress", Attr.style [("width", (toString ((progress * 100))) ++ "%")]]
    []
  ]

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
      , Events.onClick (Game (CalmDown kid))
    ] [
      table [] [
        tr [] [ td [] [ text("Rozjetost: ")], td [] [horizontalProgress [] kid.activity] ]
        , tr [] [ td [] [ text("Naštvanost: ")], td [] [horizontalProgress [] kid.frustration] ]
      ]
      , div [] [text (kid.name)]       
    ]

viewKidDialog : Kid -> Html Msg
viewKidDialog kid =
  td []
  ( 
      (
        if kid.kidDialogCooldown > 0 then 
          [ div [Attr.class "kidDialog"] [ text ( kid.shownKidDialog Texts.Cz ) ] ]
        else []
      )
      ++            
      (
        if kid.playerDialogCooldown > 0 then 
          [ div [Attr.class "playerDialog"] [ text ( kid.shownPlayerDialog Texts.Cz ) ] ]
        else []
      )
  )

view : Model -> Html Msg
view model =
  div [
     Attr.style [ nervesToStyle model.nerves ]    
  ] [
    table [] [ 
      tr [] (List.map viewKid model.kids)         
      , tr [] (List.map viewKidDialog model.kids)         
    ]    
    , div
       ( 
       [Attr.classList [
          ("gameOverlay", True)
          , ("disableGame", not (shouldUpdateGame model))
       ]]                
        ++  case model.state of
              Paused ->
                [Events.onClick (UI ResumeGame)]
              Lost _ ->
                [Events.onClick (UI RestartGame)]
              Won -> 
                [Events.onClick (UI RestartGame)]
              Running ->
                []
       )      
       (case model.state of
          Paused ->
            [text("Klikni pro spuštění")]
          Lost reason ->
            case reason of
              Activity ->
                [text("Průvodčí vás vyhodil z vlaku :-(")]
              Nerves ->
                [text("Ruply ti nervy :-(")]
          Won -> 
            [text("Dojeli jste na místo. Hurá!")]
          Running ->
            []        
       )
    , div [ 
        Attr.classList [
            ("takeDeepBreath", True)
            , ("active", model.takingDeepBreath)
            , ("highlighted", not model.takingDeepBreath && model.nerves > 1 - gameConstants.calmDownNervesGrowth)
            ]
        , Events.onMouseDown (Game DeepBreathStarted) 
        , Events.onMouseUp (Game DeepBreathEnded)
        , Events.onMouseOut (Game DeepBreathEnded)
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
          , td [] [ text("Čas do cílové stanice")]
        ]
        , tr [] [
          td [] [text("Tvoje nervy")]
          , td [] [text("Nervy průvodčího")]
          , td [] [text(
              toString ((round model.timeToWin) // 60)
              ++ ":"
              ++ Utils.fixedWidthNumberFormat 2 ( (round model.timeToWin) % 60) 
          )]
        ]
      ]
  ]