module Msg
    exposing
        ( GameMessage(..)
        , UIMessage(..)
        , MetaGameMessage(..)
        , Msg(..)
        , gameMsg
        , metaGameMsg
        , subscriptions
        )

import Time
import AnimationFrame
import Model


type GameMessage
    = DeepBreathStarted
    | DeepBreathEnded
    | CalmDownStarted Model.Kid
    | CalmDownEnded
    | ScheduleOutburst Model.OutburstParams

type MetaGameMessage
    = AddKids (List Model.Kid)
    | ReduceWaywardness (List Int) --the parameter is list of IDs
    | RemoveFrustratedKids (List Int) --the parameter is list of IDs
    | SetTimeToWin Float

type UIMessage
    =  
    ResumeGame
    | RestartGame


type Msg
    = Game GameMessage
    | Meta MetaGameMessage
    | UI UIMessage
    | Frame Time.Time



-- SUBSCRIPTIONS


gameMsg : (a -> GameMessage) -> a -> Msg
gameMsg messageConstructor value =
    Game (messageConstructor value)

metaGameMsg : (a -> MetaGameMessage) -> a -> Msg
metaGameMsg messageConstructor value =
    Meta (messageConstructor value)


subscriptions : Model.Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Frame
