module Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)

import Connected.Types exposing (Players)
import Json.Decode as Json exposing (Decoder, index, int, maybe, string)


decodePlayers : String -> Result Json.Error Players
decodePlayers msg =
    Json.decodeString playersDecoder msg


playersDecoder : Decoder Players
playersDecoder =
    Json.map2 Players
        (index 0 <| maybe string)
        (index 1 <| maybe string)


decodeDamageOutcome : String -> Result Json.Error ( Int, Int )
decodeDamageOutcome msg =
    let
        decoder : Decoder ( Int, Int )
        decoder =
            Json.map2 (\a b -> ( a, b ))
                (index 0 int)
                (index 1 int)
    in
    Json.decodeString decoder msg
