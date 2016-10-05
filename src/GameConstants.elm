module GameConstants exposing (gameConstants)

gameConstants =
  {
    activityBaseGrowth = 0.05 -- per second at max waywardness
    , activityFrustrationGrowth = 0.1 --per second at max frustration
    , frustrationGrowthFromNerves = 0.25 --growth at max nerves
    , frustrationGrowthFromNervesExponent = 1

    , nervesBaseGrowth = 0 -- per second
    , nervesActivityGrowth = 0.05 -- growth per second per kid at maximal kid activity
    , nervesActivityGrowthThreshold = 0.3
    , deepBreathNervesRecovery = 0.2 --per second

    , nervesVisualChangeThreshold = 0.4

    , calmDownNervesGrowth = 0.2
    , calmDownActivityMultiplier = 0.3 --Fraction to which activity is reduced after a calm down
    , calmDownDuration = 0.5 -- seconds
    , calmDownMutedTime = 2 -- seconds

    , dialogCooldown = 3 -- seconds

    , highActivityScoreToLose = 1
    , highActivityScoreIncreasePerKid = 0.15 -- per kid and second
    , highActivityThreshold = 0.9
    , highActivityScoreRecovery = 0.2 -- per second


    , minOutburstInterval = 3 --seconds
    , maxOutburstInterval = 12 --seconds
    , outburstActivityGrowth = 0.8 --activity growth at max waywardness

    , transitionInactivity = 0.5 -- GUI parameter (how long to prevent clicks after a transition)
  }
