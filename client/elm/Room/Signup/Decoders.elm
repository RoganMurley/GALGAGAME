module Signup.Decoders exposing (signupErrorDecoder)

import Json.Decode as Json exposing (Decoder, field, string)
import Signup.Types exposing (SignupError)


signupErrorDecoder : Decoder SignupError
signupErrorDecoder =
    Json.map SignupError (field "error" string)
