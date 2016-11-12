module KidGraphicsGeneratorTest exposing (main)

import Html.App
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Model
import RandomGenerators
import KidGenerator
import ViewGame


type alias TestModel =
    List Model.KidGraphics


type Msg
    = Refresh
    | Set (List Model.KidGraphics)


viewSingle : Bool -> Model.KidMouthState -> Model.KidGraphics -> Html Msg
viewSingle angry mouthState g =
    td [] [ ViewGame.viewKidGraphics angry mouthState g ]


viewRow : TestModel -> ( Bool, Model.KidMouthState ) -> Html Msg
viewRow model ( angry, mouthState ) =
    tr []
        (List.map (viewSingle angry mouthState) model)


view : TestModel -> Html Msg
view model =
    let
        combinations =
            [ ( False, Model.Happy )
            , ( False, Model.Neutral )
            , ( False, Model.Sad )
            , ( True, Model.Neutral )
            , ( True, Model.Sad )
            ]
    in
        table []
            (List.map (viewRow model) combinations)


generateCmd : Cmd Msg
generateCmd =
    Random.generate Set (Random.list 10 (KidGenerator.graphicsGenerator))


update : Msg -> TestModel -> ( TestModel, Cmd Msg )
update msg model =
    case msg of
        Set list ->
            ( list, Cmd.none )

        Refresh ->
            ( model, generateCmd )


main =
    Html.App.program { init = ( [], generateCmd ), view = view, update = update, subscriptions = \x -> Sub.none }
