module Connected.Decoders exposing (decodeDamageOutcome, decodeHoverOutcome, decodePlayers)

import Json.Decode as Json exposing (Decoder, index, int, maybe, string)


decodePlayers : String -> Result String ( Maybe String, Maybe String )
decodePlayers msg =
    Json.decodeString playersDecoder msg


playersDecoder : Decoder ( Maybe String, Maybe String )
playersDecoder =
    Json.map2 (,)
        (index 0 <| maybe string)
        (index 1 <| maybe string)


decodeHoverOutcome : String -> Result String (Maybe Int)
decodeHoverOutcome msg =
    Json.decodeString (maybe int) msg


decodeDamageOutcome : String -> Result String Int
decodeDamageOutcome msg =
    Json.decodeString int msg
