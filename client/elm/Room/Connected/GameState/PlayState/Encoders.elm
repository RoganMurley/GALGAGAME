module PlayState.Encoders exposing (..)

import Json.Encode exposing (encode, int)
import Json.Encode.Extra exposing (maybe)


hoverIndex : Maybe Int -> String
hoverIndex index =
    encode 0 <| maybe int index
