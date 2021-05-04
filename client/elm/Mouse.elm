module Mouse exposing (MouseState(..), Position, getVec)

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
