module Msg exposing (Msg(..), subscriptions)

import Time
import AnimationFrame
import Model exposing(..)

type Msg
  = Frame Time.Time
    | DeepBreathStarted
    | DeepBreathEnded
    | CalmDown Kid
    | ResumeGame
    | RestartGame

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Frame


