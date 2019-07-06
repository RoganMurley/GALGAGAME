module Feedback.Types exposing (FeedbackError, Field(..), Model, SubmitState(..))

import Form exposing (FormField)


type alias Model =
    { body : FormField
    , error : String
    , submitState : SubmitState
    , nextUrl : String
    }


type alias FeedbackError =
    { error : String
    }


type Field
    = Body


type SubmitState
    = NotSubmitted
    | Submitting
    | Submitted
