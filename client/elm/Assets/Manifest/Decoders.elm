module Manifest.Decoders exposing (decoder)

import Json.Decode as Json exposing (Decoder, dict, string)
import Manifest.Types exposing (Manifest)


decoder : Decoder Manifest
decoder =
    Json.dict string
