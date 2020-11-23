module Wheel.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, field)
import Wheel.Types exposing (Wheel)


decoder : Decoder a -> Decoder (Wheel a)
decoder subDecoder =
    Json.map7 (<|)
        (Json.map6 Wheel
            (field "0" subDecoder)
            (field "1" subDecoder)
            (field "2" subDecoder)
            (field "3" subDecoder)
            (field "4" subDecoder)
            (field "5" subDecoder)
        )
        (field "6" subDecoder)
        (field "7" subDecoder)
        (field "8" subDecoder)
        (field "9" subDecoder)
        (field "10" subDecoder)
        (field "11" subDecoder)
