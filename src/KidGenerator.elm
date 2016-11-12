module KidGenerator
    exposing
        ( generator
        , initGenerator
        , Gender(..)
        , graphicsGenerator
        )

import Nicknames
import Random
import RandomUtils
import Model
import Array


type Gender
    = Male
    | Female


genderGenerator : Random.Generator Gender
genderGenerator =
    Random.bool
        |> Random.map
            (\x ->
                if x then
                    Male
                else
                    Female
            )


nicknameGenerator : Gender -> Random.Generator String
nicknameGenerator gender =
    let
        nicknames =
            case gender of
                Male ->
                    Nicknames.maleNicknames

                Female ->
                    Nicknames.femaleNicknames
    in
        Random.int 0 ((Array.length nicknames) - 1)
            |> Random.map (\index -> Array.get index nicknames)
            |> Random.map (Maybe.withDefault "N/A")


waywardnessGenerator : Random.Generator Float
waywardnessGenerator =
    Random.float 0.5 1


kidFromProperties : String -> Float -> Model.Kid
kidFromProperties name waywardness =
    let
        defaultKid =
            Model.defaultKid
    in
        { defaultKid | name = name, waywardness = waywardness }


generatorKnownGender : Gender -> Random.Generator Model.Kid
generatorKnownGender gender =
    Random.map2 (kidFromProperties) (nicknameGenerator gender) (waywardnessGenerator)


generator : Random.Generator Model.Kid
generator =
    Random.andThen genderGenerator generatorKnownGender


initGenerator : Random.Generator Model.Kid
initGenerator =
    Random.map (\kid -> { kid | waywardness = kid.waywardness * 0.66 }) generator


graphicsGenerator : Random.Generator Model.KidGraphics
graphicsGenerator =
    let
        emptyGraphics =
            Model.emptyKidGraphics 
        baseGraphics =
            { emptyGraphics | body = "body00" }
            
    in
        RandomUtils.listMemberGenerator "head00" [ "head07" ]
            |> Random.map (\head -> { baseGraphics | head = head })
            |> Random.map2 (\( eyes, eyesAngry ) g -> { g | eyes = eyes, eyesAngry = eyesAngry })
                (RandomUtils.listMemberGenerator
                    ( "eyes01", "eyes01_angry" )
                    [ ( "eyes04", "eyes04_06_07_angry" )
                    , ( "eyes06", "eyes04_06_07_angry" )
                    , ( "eyes07", "eyes04_06_07_angry" )
                    ]
                )
            |> Random.map2 (\( mouthSad, mouthHappy, mouthNeutral ) g -> { g | mouthSad = mouthSad, mouthHappy = mouthHappy, mouthNeutral = mouthNeutral })
                (RandomUtils.listMemberGenerator
                    ( "mouth00_sad", "mouth00_happy", "mouth00_neutral" )
                    [ ( "mouth01_sad", "mouth01_happy", "mouth01_neutral" )
                    ]
                )
            |> Random.map2 (\scarf g -> { g | scarf = scarf })
                (RandomUtils.listMemberGenerator
                    "scarf00"
                    [ "scarf01", "scarf02" ]
                )
            |> Random.map2 (\hair g -> { g | hair = hair })
                (RandomUtils.listMemberGenerator
                    "hair00"
                    [ "hair01"
                    , "hair02"
                    , "hair03"
                    , "hair04"
                    , "hair05"
                    , "hair07"
                    , "hair08"
                    , "hair09"
                    , "hair10"
                    , "hair11"
                    , "hair12"
                    , "hair14"
                    , "hair15"
                    , "hair16"
                    , "hair17"
                    , "hair18"
                    ]
                )
            |> Random.map2 (\arm g -> { g | arm = arm })
                (RandomUtils.listMemberGenerator
                    "arm_17"
                    [ "arm_18"
                    , "arm_23"
                    , "arm_32"
                    , "arm_38"
                    ]
                )
