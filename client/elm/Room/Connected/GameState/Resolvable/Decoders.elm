module Resolvable.Decoders exposing (resolveDiffDataDecoder)

import Json.Decode as Json exposing (Decoder, field, index, list, maybe, string)
import Animation.Decoders as Animation
import Model.Diff
import Resolvable.Types exposing (..)
import Stack.Decoders as Stack


resolveDiffDataDecoder : Decoder ResolveDiffData
resolveDiffDataDecoder =
    Json.map3 (\d a s -> { diff = d, anim = a, stackCard = s })
        (index 0 Model.Diff.decoder)
        (index 1 <| maybe Animation.decoder)
        (index 2 <| maybe Stack.stackCardDecoder)
