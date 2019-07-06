module Feedback.Decoders exposing (feedbackErrorDecoder)

import Feedback.Types exposing (FeedbackError)
import Json.Decode as Json exposing (Decoder, field, string)


feedbackErrorDecoder : Decoder FeedbackError
feedbackErrorDecoder =
    Json.map FeedbackError (field "error" string)
