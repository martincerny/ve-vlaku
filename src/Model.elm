module Model exposing (
  Kid
  , OutburstParams
  , Model
  , CalmDownInfo
  , PlayerActivity(..)
  , GameState(..)
  , LostCause(..)
  , emptyOutburstParams
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
  {
    targetKidId : Int
    , interval : Float
    , intensity : Float    
  }


type alias Kid =
  {
    id : Int
    , name : String
    , waywardness : Float
    , activity: Float
    , frustration: Float
    , mutedCooldown: Float
    , shownKidDialog : Emojis.Sentence
    , kidDialogCooldown: Float
    , shownPlayerDialog : Emojis.Sentence
    , playerDialogCooldown: Float
    , timeSinceLastOutburst : Float
    , scheduledOutburst : OutburstParams
  }

emptyOutburstParams : OutburstParams
emptyOutburstParams = 
  { 
    targetKidId = -1
    , interval = 1/0
    , intensity = 0 
  }

defaultKid : Kid
defaultKid =
  { id = -1
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
  }


type LostCause = Activity | Nerves

type GameState =
  Running
  | Paused
  | Lost LostCause
  | Won

type alias CalmDownInfo =
  {
    kidId : Int
    , duration : Float
    , nervesAtStart : Float
  }

type PlayerActivity =
  None
  | DeepBreath
  | CalmDownKid CalmDownInfo

type alias Model =
  { nerves : Float
    , kids : List Kid
    , playerActivity : PlayerActivity
    , highActivityScore : Float
    , timeToOutburst : Float
    , timeToWin : Float
    , state : GameState
    , transitionInactivity : Float
  }

-- Complex modifiers

setState : Model -> GameState -> Model
setState model state =
  {model
    | state = state
    , transitionInactivity =
        if model.state == state then model.transitionInactivity
        else gameConstants.transitionInactivity
  }

-- Computed properties

shouldUpdateGame : Model -> Bool
shouldUpdateGame model =
  model.state == Running

isStateLost : GameState -> Bool
isStateLost state =
  case state of
    Lost _ -> True
    _ -> False

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
