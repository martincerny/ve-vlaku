module Utils exposing (avg, fixedWidthNumberFormat, chooseByBoolList)

import String


avg : List Float -> Float
avg list =
    (List.sum list) / (toFloat (List.length list))


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
