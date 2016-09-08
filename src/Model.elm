module Model exposing (Kid, Model, defaultKid)

type alias Kid = 
  { name : String
    , waywardness : Float
    , activity: Float
    , activityGrowthCooldown: Float   
  }

type alias Model = 
  { nerves : Float  
    , kids : List Kid
  }

defaultKid : Kid
defaultKid =
  {name = ""
  , waywardness = 0
  , activity = 0
  , activityGrowthCooldown = 0 
  }

