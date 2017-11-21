module GameState.Encoders exposing (..)

import Json.Encode exposing (encode, int)
import Json.Encode.Extra exposing (maybe)


encodeHoverIndex : Maybe Int -> String
encodeHoverIndex mIndex =
    encode 0 <| maybe int mIndex
