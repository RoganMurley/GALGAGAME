module Font.Decoders exposing (decoder)

import Dict
import Font.Types exposing (Font, FontChar)
import Json.Decode as Json exposing (Decoder, field, float)
import List
import Result
import String


decoder : Decoder Font
decoder =
    let
        charactersDecoder : List ( String, Char -> FontChar ) -> Decoder Font
        charactersDecoder pairs =
            let
                result : Result String Font
                result =
                    List.foldl
                        (\( key, value ) ->
                            Result.andThen (reduce key value)
                        )
                        (Ok Dict.empty)
                        pairs
            in
            case result of
                Ok font ->
                    Json.succeed font

                Err error ->
                    Json.fail error

        reduce : String -> (Char -> FontChar) -> Font -> Result String Font
        reduce keyString fontChar font =
            case String.uncons keyString of
                Just ( keyChar, "" ) ->
                    Ok <| Dict.insert keyChar (fontChar keyChar) font

                Just _ ->
                    Err <| "Key is not a character"

                Nothing ->
                    Err <| "Key is empty string"
    in
    Json.field "characters" (Json.keyValuePairs fontCharDecoder)
        |> Json.andThen charactersDecoder


fontCharDecoder : Decoder (Char -> FontChar)
fontCharDecoder =
    Json.map7 (\a b c d e f g h -> FontChar a b c d e f g h)
        (field "x" float)
        (field "y" float)
        (field "width" float)
        (field "height" float)
        (field "originX" float)
        (field "originY" float)
        (field "advance" float)
