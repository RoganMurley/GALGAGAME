module Signup.Messages exposing (Input(..), Msg(..))

import Http
import Login.Types exposing (LoginError)


type Msg
    = Input Input String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe LoginError))


type Input
    = Email
    | Username
    | Password
    | ConfirmPassword
