module SaveLoad exposing (encode, decode, Data)

import Json.Decode as Decode
import Json.Encode as Encode
import Model


type alias Data =
    { kids : List Model.Kid
    , numMissions : Int
    , numFailures : Int
    , numKidsAdded : Int
    , numKidsRemoved : Int
    , numKidsReducedWaywardness : Int
    , scale : Int
    }


encode : Model.Model -> Encode.Value
encode model =
    Encode.int model.gameModel.numMissions


decode : Model.Model -> Decode.Value -> Model.Model
decode currentModel storedValue =
    currentModel
