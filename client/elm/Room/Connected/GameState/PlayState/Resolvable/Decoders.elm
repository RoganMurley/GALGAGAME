module Resolvable.Decoders exposing (resolveDiffData)

import Animation.Decoders as Animation
import Json.Decode as Json exposing (Decoder, index, maybe)
import Model.Diff
import Resolvable.Types exposing (ResolveDiffData)
import Stack.Decoders as Stack


resolveDiffData : Decoder ResolveDiffData
resolveDiffData =
    Json.map3 (\d a s -> { diff = d, anim = a, stackCard = s })
        (index 0 Model.Diff.decoder)
        (index 1 Animation.decoder)
        (index 2 <| maybe Stack.stackCardDecoder)
