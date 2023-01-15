module Create.Messages exposing (Msg(..))

import Create.Types exposing (Field)


type Msg
    = SetAllowSpectators Bool
    | Input Field String
