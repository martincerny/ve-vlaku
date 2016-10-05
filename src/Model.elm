module Model exposing (
  Kid
  , Model
  , CalmDownInfo
  , PlayerActivity(..)
  , GameState(..)
  , LostCause(..)
  , defaultKid
  , shouldUpdateGame
  , isStateLost
  , isMuted
  , isKidHighActivity
  , isKidIncreasingNerves
  , setState
  )

import GameConstants exposing (..)
import Texts

type alias Kid =
  {
    id : Int
    , name : String
    , waywardness : Float
    , activity: Float
    , frustration: Float
    , mutedCooldown: Float
    , shownKidDialog : Texts.DialogString
    , kidDialogCooldown: Float
    , shownPlayerDialog : Texts.DialogString
    , playerDialogCooldown: Float
  }

defaultKid : Kid
defaultKid =
  { id = -1
  , name = ""
  , waywardness = 0
  , activity = 0
  , frustration = 0
  , mutedCooldown = 0
  , shownKidDialog = Texts.noDialogString
  , kidDialogCooldown = 0
  , shownPlayerDialog = Texts.noDialogString
  , playerDialogCooldown = 0
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

isKidIncreasingNerves : Kid -> Bool
isKidIncreasingNerves kid =
   not (isMuted kid) && kid.activity > gameConstants.nervesActivityGrowthThreshold


isKidHighActivity : Kid -> Bool
isKidHighActivity kid =
  not (isMuted kid) && kid.activity > gameConstants.highActivityThreshold


