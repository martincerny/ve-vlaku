module Msg exposing (GameMessage(..), UIMessage(..), Msg(..), subscriptions)

import Time
import AnimationFrame
import Model exposing(..)

type GameMessage =
  Frame Time.Time
  | DeepBreathStarted
  | DeepBreathEnded
  | CalmDown Kid

type UIMessage =
  ResumeGame
  | RestartGame


type Msg =
  Game GameMessage
  | UI UIMessage

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs (\time -> Game (Frame time))


