module Presence.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, int, list, string)
import Presence.Types exposing (User)


decoder : Decoder (List User)
decoder =
    field "online" <| list userDecoder


userDecoder : Decoder User
userDecoder =
    Json.map2 User
        (field "id" int)
        (field "username" string)
