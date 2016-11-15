module Model
    exposing
        ( Kid
        , KidGraphics
        , KidMouthState(..)
        , OutburstParams
        , Model
        , CalmDownInfo
        , ScheduledEvent(..)
        , PlayerActivity(..)
        , GameState(..)
        , LostCause(..)
        , emptyOutburstParams
        , emptyKidGraphics
        , defaultKid
        , shouldUpdateGame
        , isStateLost
        , isMuted
        , isKidHighActivity
        , isKidAnnoying
        , isActiveOutburst
        , setState
        )

import GameConstants exposing (..)
import Emojis


type alias OutburstParams =
    { targetKidId : Int
    , interval : Float
    , intensity : Float
    }


type ScheduledEvent
    = Unscheduled
    | Scheduled Float


type alias KidGraphics =
    { head : String
    , eyes : String
    , eyesAngry : String
    , mouthHappy : String
    , mouthSad : String
    , mouthNeutral : String
    , hair:String
    , body : String
    , scarf : String
    , arm : String
    }

type KidMouthState = Happy | Sad | Neutral

type alias Kid =
    { id : Int
    , positionId : Int
    , name : String
    , waywardness : Float
    , activity : Float
    , frustration : Float
    , mutedCooldown : Float
    , shownKidDialog : Emojis.Sentence
    , kidDialogCooldown : Float
    , shownPlayerDialog : Emojis.Sentence
    , playerDialogCooldown : Float
    , timeSinceLastOutburst : Float
    , scheduledOutburst : OutburstParams
    , frustrationRecoveryEvent : ScheduledEvent
    , numCalmDowns : Int
    , graphics: KidGraphics
    }


emptyOutburstParams : OutburstParams
emptyOutburstParams =
    { targetKidId = -1
    , interval = 1 / 0
    , intensity = 0
    }


emptyKidGraphics : KidGraphics
emptyKidGraphics =
    { head = ""
    , eyes = ""
    , eyesAngry = ""
    , mouthHappy = ""
    , mouthSad = ""
    , mouthNeutral = ""
    , hair = ""
    , body = ""
    , scarf = ""
    , arm = ""
    }


defaultKid : Kid
defaultKid =
    { id = -1
    , positionId = -1
    , name = ""
    , waywardness = 0
    , activity = 0
    , frustration = 0
    , mutedCooldown = 0
    , shownKidDialog = Emojis.nothing
    , kidDialogCooldown = 0
    , shownPlayerDialog = Emojis.nothing
    , playerDialogCooldown = 0
    , timeSinceLastOutburst = 0
    , scheduledOutburst = emptyOutburstParams
    , frustrationRecoveryEvent = Unscheduled
    , numCalmDowns = 0
    , graphics = emptyKidGraphics
    }


type LostCause
    = Activity
    | Nerves


type GameState
    = NewGame
    | Running
    | Paused
    | Lost LostCause
    | Won


type alias CalmDownInfo =
    { kidId : Int
    , positionId : Int
    , duration : Float
    , nervesAtStart : Float
    }


type PlayerActivity
    = None
    | DeepBreath
    | CalmDownKid CalmDownInfo


type alias Model =
    { nerves : Float
    , nervesTarget : Float
    , kids : List Kid
    , playerActivity : PlayerActivity
    , highActivityScore : Float
    , timeToWin : Float
    , state : GameState
    , transitionInactivity : Float
    , scale : Int
    , nextKidId : Int
    , newlyAddedKids : List Kid
    , removedFrustratedKids : List Kid
    , removedKidsAfterMissionFail : List Kid
    , kidsWithReducedWaywardness : List Kid
    , firstRun : Bool    
    }


-- Complex modifiers


setState : Model -> GameState -> Model
setState model state =
    { model
        | state = state
        , transitionInactivity =
            if model.state == state then
                model.transitionInactivity
            else
                uiConstants.transitionInactivity
    }



-- Computed properties


shouldUpdateGame : Model -> Bool
shouldUpdateGame model =
    model.state == Running


isStateLost : GameState -> Bool
isStateLost state =
    case state of
        Lost _ ->
            True

        _ ->
            False


isMuted : Kid -> Bool
isMuted kid =
    kid.mutedCooldown > 0


isKidAnnoying : Kid -> Bool
isKidAnnoying kid =
    not (isMuted kid) && kid.activity > gameConstants.annoyingActivityThreshold


isKidHighActivity : Kid -> Bool
isKidHighActivity kid =
    not (isMuted kid) && kid.activity > gameConstants.highActivityThreshold


isActiveOutburst : OutburstParams -> Bool
isActiveOutburst params =
    not (isInfinite params.interval || params.targetKidId < 0)
