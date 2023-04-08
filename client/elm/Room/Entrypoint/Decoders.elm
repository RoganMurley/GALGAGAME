module Entrypoint.Decoders exposing (decoder)

import Entrypoint.Types exposing (User)
import Json.Decode as Json exposing (Decoder, field, int, list, string)


decoder : Decoder (List User)
decoder =
    field "online" <| list userDecoder


userDecoder : Decoder User
userDecoder =
    Json.map2 User
        (field "id" int)
        (field "username" string)
