module Login.Messages exposing (..)

import Http
import Login.Types exposing (LoginError)


type Msg
    = Input Input String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe LoginError))


type Input
    = Username
    | Password
