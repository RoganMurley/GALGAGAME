module Resolvable.Decoders exposing (resolveDataDecoder)

import Json.Decode as Json exposing (Decoder, field, index, list, maybe, string)
import Animation.Decoders as Animation
import Model.Decoders exposing (modelDecoder)
import Resolvable.Types exposing (..)
import Stack.Decoders as Stack


resolveDataDecoder : Decoder ResolveData
resolveDataDecoder =
    Json.map3 (\m a s -> { model = m, anim = a, stackCard = s })
        (index 0 modelDecoder)
        (index 1 <| maybe Animation.decoder)
        (index 2 <| maybe Stack.stackCardDecoder)
