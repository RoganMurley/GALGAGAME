module Texture.Types exposing (Model)

import Dict exposing (Dict)
import WebGL.Texture exposing (Texture)


type alias Model =
    { textures : Dict String Texture
    }
