module GameConstants exposing (gameConstants)


gameConstants =
    { activityBaseGrowth =
        0.03
        --0.08 -- per second at max waywardness
    , annoyingActivityThreshold = 0.3
    , activityFrustrationGrowthThreshold =
        0.5
        --frustration over this threshold forces activity to grow
    , activityFrustrationGrowth =
        0.05
        --per second at max frustration
    , nervesBaseRecovery =
        0.0
        -- per second
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
    , calmDownActivityRecoveryHalfTime =
        0.45
        -- seconds
    , calmDownDurationFrustrationRecovery =
        0.09
        --per second
    , calmDownFrustrationRecoveryStart =
        1.1
        -- seconds
    , calmDownMutedTime =
        0.1
        -- seconds
    , calmDownFrustrationGrowthMax =
        0.3
        --growth at max nerves
    , calmDownFrustrationGrowthMin =
        0.3
        --growth at min nerves
    , calmDownFrustrationGrowthExponent = 0.8
    , frustrationRecovery =
        0.01
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
        20.0
        --mean outburst at 0 waywardness
    , meanOutburstIntervalMin =
        5.0
        --mean outburst interval at max waywardness (has to be bigger than minOutburstInterval)
    , outburstMinActivityGrowth =
        0.2
        --activity growth at min outburst intensity or waywardness
    , outburstMaxActivityGrowth =
        0.8
        --activity growth at max outburst intensity and max waywardness
    , transitionInactivity =
        0.5
        -- GUI parameter (how long to prevent clicks after a transition)
    }
