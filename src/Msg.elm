module Msg exposing (
  OutburstParams
  , GameMessage(..)
  , UIMessage(..)
  , Msg(..)
  , gameMsg
  , subscriptions)

import Time
import AnimationFrame
import Model exposing(..)

type alias OutburstParams =
  {
    target : Float -- "index" of the kid in terms of waywardness - generated with RandomGenerators.outburstTarget and resolved by RandomGenerators.outburstTargetFilter
    , intensity : Float
  }

type GameMessage =
  DeepBreathStarted
  | DeepBreathEnded
  | CalmDownStarted Kid
  | CalmDownEnded
  | ScheduleOutburst Float --parameter is the time until the outburst
  | PerformOutburst OutburstParams 

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


