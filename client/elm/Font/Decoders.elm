module Font.Decoders exposing (decoder)

import Dict
import Font.Types exposing (Font, FontChar)
import Json.Decode as Json exposing (Decoder, field, int)
import List
import Result
import String


decoder : Decoder Font
decoder =
    let
        charactersDecoder : List ( String, FontChar ) -> Decoder Font
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

        reduce : String -> FontChar -> Font -> Result String Font
        reduce keyString fontChar font =
            case String.uncons keyString of
                Just ( keyChar, "" ) ->
                    Ok <| Dict.insert keyChar fontChar font

                Just _ ->
                    Err <| "Key is not a character"

                Nothing ->
                    Err <| "Key is empty string"
    in
    Json.field "characters" (Json.keyValuePairs fontCharDecoder)
        |> Json.andThen charactersDecoder


fontCharDecoder : Decoder FontChar
fontCharDecoder =
    Json.map7 FontChar
        (field "x" int)
        (field "y" int)
        (field "width" int)
        (field "height" int)
        (field "originX" int)
        (field "originY" int)
        (field "advance" int)
