module Manifest.Decoders exposing (decoder)

import Dict
import Json.Decode as Json exposing (Decoder, string)
import Manifest.Types exposing (Manifest)


decoder : Decoder Manifest
decoder =
    Json.dict string |> Json.map (Dict.map (\_ v -> "/" ++ v))
