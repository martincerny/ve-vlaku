module Model exposing (
  Kid
  , Model
  , GameState(..)
  , defaultKid
  , shouldUpdateGame
  , nervesGrowth
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
  , shownPlayerDialog = Texts.noDialogString
  , playerDialogCooldown = 0
  }


type GameState = Running | Paused | Lost | Won

type alias Model = 
  { nerves : Float  
    , kids : List Kid
    , takingDeepBreath : Bool
    , highActivityScore : Float
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

isMuted : Kid -> Bool
isMuted kid =
  kid.mutedCooldown > 0

isKidIncreasingNerves : Kid -> Bool
isKidIncreasingNerves kid =
   not (isMuted kid) && kid.activity > gameConstants.nervesActivityGrowthThreshold

nervesGrowthPerKid : Kid -> Float
nervesGrowthPerKid kid =
  if not (isKidIncreasingNerves kid) then 0
  else
    let 
      threshold = gameConstants.nervesActivityGrowthThreshold
    in
      ((kid.activity - threshold) / (1 - threshold)) * gameConstants.nervesActivityGrowth 


nervesGrowth : Model -> Float
nervesGrowth model =
    gameConstants.nervesBaseGrowth  
    + ( List.map nervesGrowthPerKid model.kids
        |> List.sum )        

isKidHighActivity : Kid -> Bool
isKidHighActivity kid =
  not (isMuted kid) && kid.activity > gameConstants.highActivityThreshold


