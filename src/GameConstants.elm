module GameConstants exposing (gameConstants)

gameConstants =
  {
    activityBaseGrowth = 0.08 -- per second at max waywardness
    , activityFrustrationGrowth = 0 --per second at max frustration

    , nervesBaseGrowth = 0 -- per second
    , nervesActivityGrowth = 0.05 -- growth per second per kid at maximal kid activity
    , nervesActivityGrowthThreshold = 0.3 --activity level that causes a kid to increase player's nerves
    , deepBreathNervesRecovery = 0.2 --per second

    , nervesVisualChangeThreshold = 0.4 --threshold for screen reddening by increased nerves

    , calmDownNervesGrowth = 0.2
    , calmDownActivityMultiplier = 0 --Fraction to which activity is reduced after a calm down
    , calmDownDuration = 0.7 -- seconds
    , calmDownMutedTime = 2 -- seconds
    , calmDownFrustrationGrowthMax = 0.5 --growth at max nerves
    , calmDownFrustrationGrowthMin = 0.1 --growth at min nerves
    , calmDownFrustrationGrowthExponent = 0.8

    , frustrationRecovery = 0.03 -- when nothing happens, per second

    , dialogCooldown = 3 -- seconds

    , highActivityScoreToLose = 1
    , highActivityScoreIncreasePerKid = 0.11 -- per kid and second
    , highActivityThreshold = 0.9
    , highActivityScoreRecovery = 0.2 -- per second


    , minOutburstInterval = 3 --seconds
    , maxOutburstInterval = 12 --seconds
    , outburstActivityGrowth = 0.8 --activity growth at max waywardness

    , transitionInactivity = 0.5 -- GUI parameter (how long to prevent clicks after a transition)
  }
