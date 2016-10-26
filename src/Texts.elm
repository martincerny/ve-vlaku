module Texts exposing (Language(..), DialogString, noDialogString, getDialogString)

import Dialog exposing (..)
import TextsCz


type Language
    = Cz


type alias DialogString =
    Language -> String


noDialogString : DialogString
noDialogString _ =
    ""


getDialogString : Dialog -> DialogString
getDialogString dialog =
    \lang ->
        case lang of
            Cz ->
                case List.head (TextsCz.dialogStringCz dialog) of
                    Just x ->
                        x

                    Nothing ->
                        ""
