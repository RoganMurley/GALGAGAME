module Resolvable.Decoders exposing (resolveDiffData)

import Animation.Decoders as Animation
import Json.Decode as Json exposing (Decoder, field, float, index, maybe)
import Model.Diff
import Resolvable.Types exposing (ResolveDiffData)
import Stack.Decoders as Stack


resolveDiffData : Decoder ResolveDiffData
resolveDiffData =
    Json.map4 ResolveDiffData
        (field "diff" Model.Diff.decoder)
        (field "anim" Animation.decoder)
        (field "damage" <| Json.map2 (\a b -> ( a, b )) (index 0 float) (index 1 float))
        (field "stackCard" <| maybe Stack.stackCardDecoder)
