module Mouse exposing (MouseState(..), Position, decoder, getVec)

import Json.Decode as Json exposing (Decoder)
import Math.Vector2 exposing (Vec2)


type alias Position =
    { x : Int
    , y : Int
    }


type MouseState
    = NoMouse
    | Mouse Vec2
    | Touch Vec2


getVec : MouseState -> Maybe Vec2
getVec state =
    case state of
        NoMouse ->
            Nothing

        Mouse vec ->
            Just vec

        Touch vec ->
            Just vec


decoder : Decoder Position
decoder =
    Json.map2 Position
        (Json.field "x" Json.int)
        (Json.field "y" Json.int)
