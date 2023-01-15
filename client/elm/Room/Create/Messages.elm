module Create.Messages exposing (Msg(..))

import Create.Types exposing (Field)


type Msg
    = Input Field String
    | Submit
