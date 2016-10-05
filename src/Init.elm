module Init exposing (init)

import Model exposing(..)
import Msg exposing(..)
import Random
import RandomGenerators

init : (Model, Cmd Msg)
init =
  (
    {nerves = 0
    , kids = [ 
      {  defaultKid | id = 1, name = "Adam", waywardness = 1, activity = 0.21}
      , {defaultKid | id = 2, name = "Bara", waywardness = 0.8, activity = 0.15}
      , {defaultKid | id = 3, name = "Cochtan", waywardness = 0.6, activity = 0.17}
      , {defaultKid | id = 4, name = "David", waywardness = 0.4, activity = 0.05}
      , {defaultKid | id = 5, name = "Eva", waywardness = 0.2}
    ]
    , playerActivity = None
    , highActivityScore = 0
    , timeToWin = 90
    , state = Paused
    , transitionInactivity = 0
    , timeToOutburst = 1 
    }
  , Random.generate (gameMsg ScheduleOutburst)  RandomGenerators.outburstSchedule
  )
