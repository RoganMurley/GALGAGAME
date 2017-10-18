module Main.Decoders exposing (decodePlayers)

import Json.Decode as Json exposing (Decoder, index, maybe, string)


decodePlayers : String -> Result String ( Maybe String, Maybe String )
decodePlayers msg =
    Json.decodeString playersDecoder msg


playersDecoder : Decoder ( Maybe String, Maybe String )
playersDecoder =
    Json.map2 (,) (index 0 (maybe string)) (index 1 (maybe string))
