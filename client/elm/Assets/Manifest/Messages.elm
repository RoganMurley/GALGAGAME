module Manifest.Messages exposing (Msg(..))

import Manifest.Types exposing (Manifest)


type Msg
    = ManifestLoaded ( String, Manifest )
    | ManifestError String
