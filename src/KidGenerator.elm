module KidGenerator
    exposing
        ( generator
        , Gender(..)
        )

import Nicknames
import Random
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
    Random.float 0.3 1


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