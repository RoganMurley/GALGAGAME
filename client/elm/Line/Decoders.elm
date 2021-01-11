module Line.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, float)
import Line.Types exposing (Line)


decoder : Decoder Line
decoder =
    let
        startDecoder =
            Json.map2 Tuple.pair
                (Json.index 0 float)
                (Json.index 1 float)

        endDecoder =
            Json.map2 Tuple.pair
                (Json.index 0 float)
                (Json.index 1 float)

        makeLine : ( Float, Float ) -> ( Float, Float ) -> Line
        makeLine ( x1, y1 ) ( x2, y2 ) =
            { x1 = x1
            , y1 = y1
            , x2 = x2
            , y2 = y2
            }
    in
    Json.map2 makeLine
        (Json.index 0 startDecoder)
        (Json.index 1 endDecoder)
