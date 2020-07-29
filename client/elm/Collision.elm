module Collision exposing (hitTest)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


hitTest3D : Vec3 -> Float -> { a | position : Vec3 } -> Bool
hitTest3D pos dist { position } =
    False
