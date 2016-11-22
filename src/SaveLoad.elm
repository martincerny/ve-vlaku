port module SaveLoad
    exposing
        ( encodeGame
        , gameDecoder
        , encodeSettings
        , settingsDecoder
        , saveGame
        , saveSettings
        )

import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Model


port saveGame : Encode.Value -> Cmd msg


port saveSettings : Encode.Value -> Cmd msg


encodeSettings : Model.Model -> Encode.Value
encodeSettings model =
    Encode.object
        [ ( "scale", Encode.int model.scale ) ]


applyDecodedSettings : Model.Model -> Int -> Model.Model
applyDecodedSettings model scale =
    { model | scale = scale }


settingsDecoder : Model.Model -> Decode.Decoder Model.Model
settingsDecoder currentModel =
    Decode.object1 (applyDecodedSettings currentModel)
        ("scale" := Decode.int)


encodeGame : Model.GameModel -> Encode.Value
encodeGame model =
    Encode.object
        [ ( "kids", Encode.list (List.map encodeKid model.kids) )
        , ( "numMissions", Encode.int model.numMissions )
        , ( "numFailures", Encode.int model.numFailures )
        , ( "numKidsAdded", Encode.int model.numKidsAdded )
        , ( "numKidsRemoved", Encode.int model.numKidsRemoved )
        , ( "numKidsReducedWaywardness", Encode.int model.numKidsReducedWaywardness )
        ]


applyDecodedData : Model.GameModel -> List Model.Kid -> Int -> Int -> Int -> Int -> Int -> Model.GameModel
applyDecodedData currentModel kids numMissions numFailures numKidsAdded numKidsRemoved numKidsReducedWaywardness =
    { currentModel
        | kids = kids
        , numMissions = numMissions
        , numFailures = numFailures
        , numKidsAdded = numKidsAdded
        , numKidsRemoved = numKidsRemoved
        , numKidsReducedWaywardness = numKidsReducedWaywardness
    }


gameDecoder : Model.GameModel -> Decode.Decoder Model.GameModel
gameDecoder currentModel =
    Decode.object6 (applyDecodedData currentModel)
        ("kids" := Decode.list kidDecoder)
        ("numMissions" := Decode.int)
        ("numFailures" := Decode.int)
        ("numKidsAdded" := Decode.int)
        ("numKidsRemoved" := Decode.int)
        ("numKidsReducedWaywardness" := Decode.int)


encodeKid : Model.Kid -> Encode.Value
encodeKid kid =
    Encode.object
        [ ( "id", Encode.int kid.id )
        , ( "name", Encode.string kid.name )
        , ( "waywardness", Encode.float kid.waywardness )
        , ( "graphics", encodeKidGraphics kid.graphics )
        ]


kidFromDecodedData : Int -> String -> Float -> Model.KidGraphics -> Model.Kid
kidFromDecodedData id name waywardness graphics =
    let
        kid =
            Model.defaultKid
    in
        { kid
            | id = id
            , name = name
            , waywardness = waywardness
            , graphics = graphics
        }


kidDecoder : Decode.Decoder Model.Kid
kidDecoder =
    Decode.object4 kidFromDecodedData
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("waywardness" := Decode.float)
        ("graphics" := kidGraphicsDecoder)


encodeKidGraphics : Model.KidGraphics -> Encode.Value
encodeKidGraphics g =
    Encode.object
        (List.map (\( name, value ) -> ( name, Encode.string value ))
            [ ( "head", g.head )
            , ( "eyes", g.eyes )
            , ( "eyesAngry", g.eyesAngry )
            , ( "mouthHappy", g.mouthHappy )
            , ( "mouthSad", g.mouthSad )
            , ( "mouthNeutral", g.mouthNeutral )
            , ( "hair", g.hair )
            , ( "body", g.body )
            , ( "scarf", g.scarf )
            , ( "arm", g.arm )
            ]
        )


kidGraphicsFromDecodedData : ( String, String, String ) -> ( String, String, String ) -> ( String, String, String, String ) -> Model.KidGraphics
kidGraphicsFromDecodedData ( head, eyes, eyesAngry ) ( mouthHappy, mouthSad, mouthNeutral ) ( hair, body, scarf, arm ) =
    { head = head
    , eyes = eyes
    , eyesAngry = eyesAngry
    , mouthHappy = mouthHappy
    , mouthSad = mouthSad
    , mouthNeutral = mouthNeutral
    , hair = hair
    , body = body
    , scarf = scarf
    , arm = arm
    }


kidGraphicsDecoder : Decode.Decoder Model.KidGraphics
kidGraphicsDecoder =
    Decode.object3 kidGraphicsFromDecodedData
        (Decode.object3 (,,)
            ("head" := Decode.string)
            ("eyes" := Decode.string)
            ("eyesAngry" := Decode.string)
        )
        (Decode.object3 (,,)
            ("mouthHappy" := Decode.string)
            ("mouthSad" := Decode.string)
            ("mouthNeutral" := Decode.string)
        )
        (Decode.object4 (,,,)
            ("hair" := Decode.string)
            ("body" := Decode.string)
            ("scarf" := Decode.string)
            ("arm" := Decode.string)
        )
