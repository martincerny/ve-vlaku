module Init exposing (init, initGame)

import Model
import Msg
import Random
import KidGenerator
import RandomGenerators
import Json.Decode exposing ((:=))
import SaveLoad


initialKids : Int
initialKids =
    4


initGame : ( Model.GameModel, Cmd Msg.Msg )
initGame =
    { nerves = 0
    , nervesTarget = 0
    , kids = []
    , playerActivity = Model.None
    , highActivityScore = 0
    , timeToWin = 90
    , state = Model.Running
    , nextKidId = 0
    , numMissions = 0
    , numFailures = 0
    , numKidsAdded = 0
    , numKidsRemoved = 0
    , numKidsReducedWaywardness = 0
    }
        ! [ Random.generate (Msg.metaGameMsg Msg.AddKids) (KidGenerator.initGenerator initialKids)
          , Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin initialKids)
          ]


cmdsAfterLoad : Model.GameModel -> Cmd Msg.Msg
cmdsAfterLoad loadedGame =
    Cmd.none


type alias Flags =
    { settings : Json.Decode.Value
    , state : Json.Decode.Value
    , query : String
    }


init : Flags -> ( Model.Model, Cmd Msg.Msg )
init flags =
    let
        ( defaultGameModel, defaultCmd ) =
            initGame

        defaultMainModel =
            { uiState = Model.MainMenu
            , transitionInactivity = 0
            , allowScale = True
            , scale = 1
            , newlyAddedKids = []
            , removedFrustratedKids = []
            , removedKidsAfterMissionFail = []
            , kidsWithReducedWaywardness = []
            , gameModel = gameModel
            }

        resultStoredSettings =
            Json.Decode.decodeValue (SaveLoad.settingsDecoder defaultMainModel) flags.settings

        resultStoredState =
            Json.Decode.decodeValue (SaveLoad.gameDecoder defaultGameModel) flags.state

        ( allowScale, defaultScale ) =
            case flags.query of
                "noScale" ->
                    ( False, 1 )

                "full" ->
                    ( True, 2 )

                _ ->
                    ( True, 1 )

        ( gameModel, cmd ) =
            case resultStoredState of
                Ok model ->
                    ( model, cmdsAfterLoad model )

                Err _ ->
                    ( defaultGameModel, defaultCmd )

        mainModel =
            case resultStoredSettings of
                Ok model ->
                    model

                Err _ ->
                    { defaultMainModel | scale = defaultScale }
    in
        ( { mainModel
            | gameModel = gameModel
            , allowScale = allowScale
          }
        , cmd
        )
