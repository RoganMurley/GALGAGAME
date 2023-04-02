module Presence.Decoders exposing (decoder)

import Json.Decode exposing (Decoder, field, list, string)


decoder : Decoder (List String)
decoder =
    field "online" <| list string
