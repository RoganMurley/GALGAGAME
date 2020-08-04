module Collision exposing (Ray, hitTest, hitTest3d)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


type alias Ray =
    { origin : Vec3
    , direction : Vec3
    }


hitTest3d : Ray -> Float -> { a | position : Vec3 } -> Bool
hitTest3d { origin, direction } radius { position } =
    let
        oc =
            Vector3.sub origin position

        a =
            Vector3.dot direction direction

        b =
            2 * Vector3.dot oc direction

        c =
            Vector3.dot oc oc - (radius * radius)

        discriminant =
            b * b - 4 * a * c
    in
    discriminant > 0
