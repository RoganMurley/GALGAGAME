module Feedback.Messages exposing (Msg(..))

import Feedback.Types exposing (Field)
import Http
import Login.Types exposing (LoginError)


type Msg
    = Input Field String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe LoginError))
    | Continue
