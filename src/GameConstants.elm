module GameConstants exposing (gameConstants, uiConstants, metaGameConstants)


gameConstants =
    { activityBaseGrowth =
        0.03
        --0.08 -- per second at max waywardness
    , annoyingActivityThreshold = 0.3
    , activityFrustrationGrowthThreshold =
        0.5
        --frustration over this threshold forces activity to grow
    , activityFrustrationGrowth =
        0.07
        --per second at max frustration
    , nervesBaseRecovery =
        0.0
        -- per second
    , nervesTargetFollowingHalfTime =
        0.2
        --per second
    , nervesActivityGrowth =
        0.0
        -- growth per second per kid at maximal kid activity
    , deepBreathNervesRecovery =
        0.4
        --per second
    , nervesVisualChangeThreshold =
        0.4
        --threshold for screen reddening by increased nerves
    , calmDownNervesGrowthCoefficient =
        0.2
        --0.17
    , calmDownFrustrationRecovery =
        0.18
        --per each recovery event
    , calmDownFrustrationRecoveryStart =
        1.1
        -- seconds
    , calmDownFrustrationRecoveryMinInterval =
        0.7
        -- seconds
    , calmDownFrustrationRecoveryMeanInterval =
        2
        --seconds
    , calmDownMutedTime =
        0.1
        -- seconds
    , calmDownFrustrationGrowth =
        0.5
    , frustrationRecovery =
        0.00
        -- when nothing happens, per second
    , dialogCooldown =
        3
        -- seconds
    , highActivityScoreToLose = 1
    , highActivityScoreIncreasePerKid =
        0.11
        -- per kid and second
    , highActivityThreshold = 0.9
    , highActivityScoreRecovery =
        0.15
        -- per second
    , minOutburstInterval =
        2
        --minimal time between two outbursts (seconds)
    , meanOutburstIntervalMax =
        45.0
        --mean outburst at 0 waywardness
    , meanOutburstIntervalMin =
        3.0
        --mean outburst interval at max waywardness (has to be bigger than minOutburstInterval)
    , outburstMinActivityGrowth =
        0.1
        --activity growth at min outburst intensity or waywardness
    , outburstMaxActivityGrowth =
        0.9
        --activity growth at max outburst intensity and max waywardness
    }


uiConstants =
    { transitionInactivity =
        0.5
        -- GUI parameter (how long to prevent clicks after a transition)
    }


metaGameConstants =
    { maximalMeanFrustrationToAddKid =
        0.5
    , maximalChanceOfAddingKid =
        0.9
        --at 0 mean frustration
    , minFrustrationToConsiderRemovingKid =
        0.7
    , maxChanceOfRemovingKidForFrustration =
        0.8
    , numKidsFor50PercentChanceRemovalAfterMissionFail =
        4
    , minKidsToKeep =
        3
    , maxFrustrationToConsiderReducingWaywardness =
        0.2
    , maxChanceOfReducingWaywardness =
        0.8
    , waywardnessReduction =
        0.2
        --The reduction after the kid completes with little frustration
    , minimalWaywardness =
        0.2
    }
