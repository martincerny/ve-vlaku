module Init exposing (init)

import Model
import Msg
import Random
import KidGenerator
import RandomGenerators


initialKids : Int
initialKids =
    4


init : ( Model.Model, Cmd Msg.Msg )
init =
    { nerves = 0
    , nervesTarget = 0
    , kids = []
    , playerActivity = Model.None
    , highActivityScore = 0
    , timeToWin = 90
    , state = Model.NewGame
    , transitionInactivity = 0
    , scale = 2
    , nextKidId = 0
    , newlyAddedKids = []
    , removedFrustratedKids = []
    , removedKidsAfterMissionFail = []
    , kidsWithReducedWaywardness = []
    , firstRun = True
    }
        ! [ Random.generate (Msg.metaGameMsg Msg.AddKids) (Random.list initialKids KidGenerator.initGenerator)
          , Random.generate (Msg.metaGameMsg Msg.SetTimeToWin) (RandomGenerators.timeToWin initialKids)
          ]
