module TextsCz exposing (dialogStringCz)

import Dialog exposing(..)

dialogStringCz: Dialog -> List String
dialogStringCz dialog =
  case dialog of
    CalmDownLowNerves -> ["Uklidni se, prosím."]
    CalmDownMidNerves -> ["Hej, fakt to klidni!"]
    CalmDownHighNerves -> ["Drž už konečně hubu!"]

    OutburstLow -> ["Hele, srnka!"]
    OutburstHigh -> ["Pojďme se poprat"]

