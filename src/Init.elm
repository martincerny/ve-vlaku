module Init exposing (init, initGame)

import Model
import Msg
import Random
import KidGenerator
import RandomGenerators


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


init : ( Model.Model, Cmd Msg.Msg )
init =
    let
        ( gameModel, cmd ) =
            initGame
    in
        ( { uiState = Model.MainMenu
          , transitionInactivity = 0
          , scale = 1
          , newlyAddedKids = []
          , removedFrustratedKids = []
          , removedKidsAfterMissionFail = []
          , kidsWithReducedWaywardness = []
          , gameModel = gameModel
          }
        , cmd
        )
