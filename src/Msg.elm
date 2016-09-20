module Msg exposing (GameMessage(..), UIMessage(..), Msg(..), gameMsg, subscriptions)

import Time
import AnimationFrame
import Model exposing(..)

type GameMessage =
  DeepBreathStarted
  | DeepBreathEnded
  | CalmDown Kid
  | ScheduleOutburst Float --parameter is the time until the outburst
  | PerformOutburst Float --parameter is the "index" of the kid in terms of waywardness

type UIMessage =
  ResumeGame
  | RestartGame


type Msg =
  Game GameMessage
  | UI UIMessage
  | Frame Time.Time

-- SUBSCRIPTIONS

gameMsg : (a -> GameMessage) -> a -> Msg
gameMsg messageConstructor value =
  Game (messageConstructor value)

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.diffs Frame


