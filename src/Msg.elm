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
import Keyboard


type GameMessage
    = DeepBreathStarted
    | DeepBreathEnded
    | CalmDownStarted Model.Kid
    | CalmDownEnded
    | ScheduleOutburst Model.OutburstParams
    | ScheduleFrustrationRecovery Model.Kid Float
    | InitFrustration Model.Kid Float 
    | InitActivity Model.Kid Float 

type MetaGameMessage
    = AddKids (List Model.Kid)
    | ReduceWaywardness (List Model.Kid) 
    | RemoveFrustratedKids (List Model.Kid)
    | RemoveKidsAfterMissionFail (List Model.Kid)
    | SetTimeToWin Float

type UIMessage
    = StartNewGame
    | StartTutorial
    | TutorialNext
    | StartMission
    | PauseMission 
    | ResumeMission
    | MissionBriefing
    | ShowMainMenu
    | SetScale Int
    | KeyPressed Keyboard.KeyCode


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

uiMsg : (a -> UIMessage) -> a -> Msg
uiMsg messageConstructor value =
    UI (messageConstructor value)


subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Sub.batch [AnimationFrame.diffs Frame, Keyboard.presses (uiMsg KeyPressed) ] 
