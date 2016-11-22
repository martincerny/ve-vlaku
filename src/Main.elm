module Main exposing (..)

import Html.App as App
--import TimeTravel.Html.App as App

import Update
import View
import Msg exposing (Msg)
import Init exposing (..)
import Json.Encode

main =
    App.programWithFlags
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = Msg.subscriptions
        }



-- MODEL
