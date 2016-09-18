module Texts exposing (Language(..), DialogString, noDialogString, getDialogString, calmDownDialog)

import Dialog exposing(..)
import TextsCz

type Language = Cz 


type alias DialogString =
  Language -> String

noDialogString : DialogString
noDialogString _ =
  ""  

getDialogString: Dialog -> DialogString
getDialogString dialog =
  \lang -> case lang of
            Cz -> case List.head (TextsCz.dialogStringCz dialog) of
                    Just x -> x
                    Nothing -> ""

calmDownDialog : Float -> Dialog
calmDownDialog nerves =
  if nerves < 0.5 then CalmDownLowNerves
  else if nerves < 0.9 then CalmDownMidNerves
  else CalmDownHighNerves