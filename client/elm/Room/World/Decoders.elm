module World.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field, float, list, string)
import World.Types exposing (Encounter, World)


decoder : Decoder World
decoder =
    let
        posDecoder =
            Json.map2 Tuple.pair
                (Json.index 0 float)
                (Json.index 1 float)
    in
    Json.map2 World
        (field "encounters" <| list encounterDecoder)
        (field "others" <| list posDecoder)


encounterDecoder : Decoder Encounter
encounterDecoder =
    Json.map5 Encounter
        (field "guid" string)
        (field "name" string)
        (field "numeral" string)
        (field "x" float)
        (field "y" float)
