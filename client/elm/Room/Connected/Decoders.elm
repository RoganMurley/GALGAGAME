module Connected.Decoders exposing (decodeDamageOutcome, decodePlayers)

import Json.Decode as Json exposing (Decoder, index, int, maybe, string)


decodePlayers : String -> Result String ( Maybe String, Maybe String )
decodePlayers msg =
    Json.decodeString playersDecoder msg


playersDecoder : Decoder ( Maybe String, Maybe String )
playersDecoder =
    Json.map2 (,)
        (index 0 <| maybe string)
        (index 1 <| maybe string)


decodeDamageOutcome : String -> Result String ( Int, Int )
decodeDamageOutcome msg =
    let
        decoder : Decoder ( Int, Int )
        decoder =
            Json.map2 (,)
                (index 0 int)
                (index 1 int)
    in
    Json.decodeString decoder msg
