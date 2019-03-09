module Signup.Messages exposing (Msg(..))

import Http
import Login.Types exposing (LoginError)
import Signup.Types exposing (Field)


type Msg
    = Input Field String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe LoginError))
