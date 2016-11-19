module Utils exposing (avg, fixedWidthNumberFormat, chooseByBoolList, listGet, timeToMinSec)

import String


avg : List Float -> Float
avg list =
    (List.sum list) / (toFloat (List.length list))


listGet : Int -> List a -> Maybe a
listGet index list =
    if index < 0 then
        Nothing
    else
        case list of
            head :: tail ->
                if index == 0 then
                    Just head
                else
                    listGet (index - 1) tail

            [] ->
                Nothing


chooseByBoolList : List Bool -> List a -> List a
chooseByBoolList boolList list =
    case boolList of
        boolHead :: boolTail ->
            case list of
                head :: tail ->
                    if (boolHead) then
                        head :: (chooseByBoolList boolTail tail)
                    else
                        (chooseByBoolList boolTail tail)

                [] ->
                    []

        [] ->
            []


fixedWidthNumberFormat : Int -> Int -> String
fixedWidthNumberFormat width number =
    String.padLeft width '0' (toString number)


timeToMinSec : Float -> String
timeToMinSec time =
    toString ((round time) // 60)
        ++ ":"
        ++ fixedWidthNumberFormat 2 ((round time) % 60)
