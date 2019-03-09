module Login.Messages exposing (Msg(..))

import Http
import Login.Types exposing (Field, LoginError)


type Msg
    = Input Field String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe LoginError))
