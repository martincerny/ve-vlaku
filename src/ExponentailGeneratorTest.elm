module ExponentialGeneratorTest exposing (main)

import Html.App
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Array
import RandomGenerators

type alias Model = List Float

type  Msg = Refresh | Set (List Float) 

binCount : Int
binCount = 30


countValues : Float -> Float -> List Float -> Int
countValues min max list =
    list 
    |> List.filter (\x -> x >= min && x < max)
    |> List.length 


viewBin : Float -> Float -> Int -> Int -> Html Msg
viewBin min max index count =
    let     
      visualWidth = (100 / (toFloat binCount))
      left = visualWidth * (toFloat index)
    in      
    div [Attr.style [
        ("position", "absolute")
        , ("top", (toString ((toFloat (300 - count)) / 3)) ++ "%")        
        , ("bottom", "0%")
        , ("left", (toString left) ++ "%" )
        , ("width", (toString (100 / toFloat binCount)) ++ "%" )
        , ("background-color","lightblue")
    ]
    ][ text( toString count)]

view : Model -> Html Msg 
view model =
 let 
    minX = List.minimum model |> Maybe.withDefault 0
    maxX = List.maximum model |> Maybe.withDefault 30
    binWidth = (maxX - minX) / toFloat binCount 
    initFunc = \index -> countValues (minX + binWidth * (toFloat index)) (minX + binWidth * (toFloat index + 1)) model 
    binned = Array.initialize binCount initFunc  
 in
    div [ Events.onClick Refresh,
    Attr.style [("position", "absolute"), ("width","100%"), ("height","100%")] ]
    (
       Array.indexedMap (viewBin minX maxX) binned
       |> Array.toList
    )

generateCmd : Cmd Msg 
generateCmd = 
    Random.generate Set (Random.list 1000 (RandomGenerators.exponentialGenerator 5 30))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Set list ->
            (list, Cmd.none)
        Refresh ->
            (model , generateCmd)        

main = 
    Html.App.program { init = ([10,11,12], generateCmd), view = view, update = update, subscriptions = \x -> Sub.none } 
