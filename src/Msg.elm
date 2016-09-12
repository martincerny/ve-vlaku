module Msg exposing (GameMessage(..), UIMessage(..), Msg(..), subscriptions)

import Time
import AnimationFrame
import Model exposing(..)

type GameMessage =
  DeepBreathStarted
  | DeepBreathEnded
  | CalmDown Kid

type UIMessage =
  ResumeGame
  | RestartGame


type Msg =
  Game GameMessage
  | UI UIMessage
  | Frame Time.Time

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Frame


