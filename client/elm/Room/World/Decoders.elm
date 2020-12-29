module World.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, list, string)
import World.Types exposing (Encounter, World)


decoder : Decoder World
decoder =
    list encounterDecoder


encounterDecoder : Decoder Encounter
encounterDecoder =
    Json.map4 Encounter
        (field "guid" string)
        (field "name" string)
        (field "x" float)
        (field "y" float)
