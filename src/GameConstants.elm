module GameConstants exposing (gameConstants)

gameConstants =
  { activityBaseGrowth = 0.15 -- per second
    , activityGrowthCooldown = 1 -- seconds
    --, activity
    , nervesBaseGrowth = 0 -- per second
    , nervesActivityGrowth = 0.2 -- growth per second at maximal kid activity    
    , nervesActivityGrowthThreshold = 0.3
    , deepBreathNervesRecovery = 0.2 --per second

    , nervesVisualChangeThreshold = 0.6

    , calmDownNervesGrowth = 0.1
    , calmDownActivityMultiplier = 0.5   


    , highActivityKidsToFail = 2 -- no. of kids
    , highActivityTimeToFail = 3 -- seconds
    , highActivityThreshold = 0.9

  }