module Resolvable.Decoders exposing (resolveDiffData)

import Animation.Decoders as Animation
import Json.Decode as Json exposing (Decoder, field, float, index)
import Model.Diff
import Resolvable.Types exposing (ResolveDiffData)


resolveDiffData : Decoder ResolveDiffData
resolveDiffData =
    Json.map3 ResolveDiffData
        (field "diff" Model.Diff.decoder)
        (field "anim" Animation.decoder)
        (field "damage" <| Json.map2 (\a b -> ( a, b )) (index 0 float) (index 1 float))
