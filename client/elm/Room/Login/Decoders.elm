module Login.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, field, string)
import Login.Types exposing (LoginError)


loginErrorDecoder : Decoder LoginError
loginErrorDecoder =
    Json.map LoginError (field "error" string)
