module Msg
    exposing
        ( GameMessage(..)
        , UIMessage(..)
        , Msg(..)
        , gameMsg
        , subscriptions
        )

import Time
import AnimationFrame
import Model exposing (..)


type GameMessage
    = DeepBreathStarted
    | DeepBreathEnded
    | CalmDownStarted Kid
    | CalmDownEnded
    | ScheduleOutburst OutburstParams


type UIMessage
    = ResumeGame
    | RestartGame


type Msg
    = Game GameMessage
    | UI UIMessage
    | Frame Time.Time



-- SUBSCRIPTIONS


gameMsg : (a -> GameMessage) -> a -> Msg
gameMsg messageConstructor value =
    Game (messageConstructor value)


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Frame
