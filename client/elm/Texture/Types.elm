module Texture.Types exposing (..)

import Dict exposing (Dict)
import WebGL.Texture exposing (Texture)


type alias Model =
    { textures : Dict String Texture
    }
