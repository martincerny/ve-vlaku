import Html.App as App
--import TimeTravel.Html.App as App

import Model exposing (..)
import Update
import View
import Msg exposing (Msg)
import Init exposing (..)

main =
  App.program
    { init = init
    , view = View.view
    , update = Update.update
    , subscriptions = Msg.subscriptions
    }


-- MODEL







  