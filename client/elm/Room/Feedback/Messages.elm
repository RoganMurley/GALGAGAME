module Feedback.Messages exposing (Msg(..))

import Feedback.Types exposing (FeedbackError, Field)
import Http


type Msg
    = Input Field String
    | Submit
    | SubmitCallback (Result Http.Error (Maybe FeedbackError))
    | Continue
