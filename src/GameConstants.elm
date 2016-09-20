module GameConstants exposing (gameConstants)

gameConstants =
  {   
    activityBaseGrowth = 0.1 -- per second at max waywardness
    , activityFrustrationGrowth = 0.1 --per second at max frustration
    , frustrationGrowthFromNerves = 0.5 --growth at max nerves
    , frustrationGrowthFromNervesExponent = 1

    , nervesBaseGrowth = 0 -- per second
    , nervesActivityGrowth = 0.05 -- growth per second per kid at maximal kid activity    
    , nervesActivityGrowthThreshold = 0.3
    , deepBreathNervesRecovery = 0.2 --per second

    , nervesVisualChangeThreshold = 0.4

    , calmDownNervesGrowth = 0.1
    , calmDownActivityMultiplier = 0.5   
    , calmDownMutedTime = 2 -- seconds

    , dialogCooldown = 3 -- seconds

    , highActivityScoreToLose = 1
    , highActivityScoreIncreasePerKid = 0.15 -- per kid and second    
    , highActivityThreshold = 0.9
    , highActivityScoreRecovery = 0.3 -- per second


    , minOutburstInterval = 1 --seconds
    , maxOutburstInterval = 5 --seconds
    , outburstActivityGrowth = 0.7 --activity growth at max waywardness

    , transitionInactivity = 0.5
  }