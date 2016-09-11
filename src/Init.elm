module Init exposing (init)

import Model exposing(..)
import Msg exposing(..)

init : (Model, Cmd Msg)
init =
  (
    {nerves = 0
    , kids = [ 
      {  defaultKid | id = 1, name = "Adam", waywardness = 0.1, activity = 0.21}
      , {defaultKid | id = 2, name = "Bara", waywardness = 0.3, activity = 0.15}
      , {defaultKid | id = 3, name = "Cochtan", waywardness = 0.5, activity = 0.17}
      , {defaultKid | id = 4, name = "David", waywardness = 0.4, activity = 0.05}
      , {defaultKid | id = 5, name = "Eva", waywardness = 0.2}
    ]
    , takingDeepBreath = False
    , highActivityScore = 0
    , timeToWin = 90
    , state = Paused 
    }
  , Cmd.none
  )
