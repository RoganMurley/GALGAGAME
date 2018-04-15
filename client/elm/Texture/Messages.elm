module Texture.Messages exposing (Msg(..))

import WebGL.Texture exposing (Error, Texture)


type Msg
    = TexturesLoaded (List ( String, Texture ))
    | TexturesError Error
