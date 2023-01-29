module Quest.Decoders exposing (..)

import Json.Decode as Json exposing (Decoder, field, int, string)
import Quest.Types exposing (Quest)


decoder : Decoder Quest
decoder =
    Json.map3 Quest
        (field "name" string)
        (field "desc" string)
        (field "xp" int)
