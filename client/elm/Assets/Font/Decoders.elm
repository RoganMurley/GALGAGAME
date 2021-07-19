module Font.Decoders exposing (decoder)

import Dict exposing (Dict)
import Font.Types exposing (Font, FontChar)
import Json.Decode as Json exposing (Decoder, field, float, index)
import List
import Result
import String


decoder : Decoder Font
decoder =
    let
        charactersDecoder : List ( String, Char -> FontChar ) -> Decoder (Dict Char FontChar)
        charactersDecoder pairs =
            let
                result : Result String (Dict Char FontChar)
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

        reduce : String -> (Char -> FontChar) -> Dict Char FontChar -> Result String (Dict Char FontChar)
        reduce keyString fontChar font =
            case String.uncons keyString of
                Just ( keyChar, "" ) ->
                    Ok <| Dict.insert keyChar (fontChar keyChar) font

                Just _ ->
                    Err <| "Key is not a character"

                Nothing ->
                    Err <| "Key is empty string"
    in
    Json.map2 Font
        (Json.field "characters" (Json.keyValuePairs fontCharDecoder)
            |> Json.andThen charactersDecoder
        )
        (Json.field "kerning" kerningDecoder)


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


type alias Kern =
    { a : String
    , b : String
    , kern : Float
    }


kerningDecoder : Decoder (Dict ( Char, Char ) Float)
kerningDecoder =
    let
        makeKern : Kern -> Decoder ( ( Char, Char ), Float )
        makeKern { a, b, kern } =
            case ( String.uncons a, String.uncons b ) of
                ( Just ( charA, _ ), Just ( charB, _ ) ) ->
                    Json.succeed <| ( ( charA, charB ), kern )

                _ ->
                    Json.fail <| "Invalid kernings"
    in
    Json.map Dict.fromList <|
        Json.list <|
            Json.andThen makeKern <|
                Json.map3 Kern
                    (index 0 Json.string)
                    (index 1 Json.string)
                    (index 2 float)
