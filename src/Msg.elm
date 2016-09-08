module Msg exposing (Msg(..), subscriptions)

import Time
import AnimationFrame
import Model exposing(Model)

type Msg
  = Frame Time.Time
    | DeepBreathStarted
    | DeepBreathEnded

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Frame


