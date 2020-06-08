module Collision exposing (hitTest)

import Math.Vector2 exposing (Vec2)


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist
