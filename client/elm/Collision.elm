module Collision exposing (AABB, Ray, hitAABB, hitTest, hitTest3d)

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


type alias AABB =
    { r1 : Vec2
    , r2 : Vec2
    }


hitAABB : AABB -> Vec2 -> Bool
hitAABB aabb point =
    let
        p =
            Math.Vector2.toRecord point

        r1 =
            Math.Vector2.toRecord aabb.r1

        r2 =
            Math.Vector2.toRecord aabb.r2
    in
    p.x >= r1.x && p.x <= r2.x && p.y >= r1.y && p.y <= r2.y
