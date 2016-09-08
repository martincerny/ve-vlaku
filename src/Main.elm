import Html.App as App
--import TimeTravel.Html.App as App

import Model exposing (..)
import Update
import View
import Msg exposing (Msg)


main =
  App.program
    { init = init
    , view = View.view
    , update = Update.update
    , subscriptions = Msg.subscriptions
    }


-- MODEL



init : (Model, Cmd Msg)
init =
  (
    {nerves = 0
    , kids = [ 
      {defaultKid | name = "Adam", waywardness = 0.1}
      , {defaultKid | name = "Bara", waywardness = 0.3}
      , {defaultKid | name = "Cochtan", waywardness = 0.8}
      , {defaultKid | name = "David", waywardness = 0.5}
      , {defaultKid | name = "Eva", waywardness = 0.2}
    ]
    , takingDeepBreath = False
    , lost = False
    , highActivityTime = 0 
    }
  , Cmd.none
  )




  