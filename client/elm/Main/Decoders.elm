module Main.Decoders exposing (decodePlayers)

import Json.Decode as Json exposing (Decoder, index, maybe, string)


decodePlayers : String -> ( Maybe String, Maybe String )
decodePlayers msg =
    case Json.decodeString playersDecoder msg of
        Ok result ->
            result

        Err err ->
            Debug.crash err


playersDecoder : Decoder ( Maybe String, Maybe String )
playersDecoder =
    Json.map2 (,) (index 0 (maybe string)) (index 1 (maybe string))
