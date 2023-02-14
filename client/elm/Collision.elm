module Collision exposing (AABB, Ray, hitAABB, hitTest, hitTest3d, hitTest3dTri)

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
    -- Adapted from scratchapixel
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


hitTest3dTri : Ray -> Vec3 -> Vec3 -> Vec3 -> Bool
hitTest3dTri { origin, direction } v0 v1 v2 =
    -- Möller-Trumbore algorithm adapted from ChatGPT and scratchapixel
    let
        v0v1 =
            Vector3.sub v1 v0

        v0v2 =
            Vector3.sub v2 v0

        --
        pvec =
            Vector3.cross direction v0v2

        det =
            Vector3.dot v0v1 pvec
    in
    -- If det is close to zero, the ray is parallel to the plane
    -- and there won't be an intersection
    if abs det < 0.00001 then
        False

    else
        let
            invDet =
                1 / det

            tvec =
                Vector3.sub origin v0

            u =
                invDet * Vector3.dot tvec pvec

            -- If u is not between 0 and 1, the intersection is outside the triangle
        in
        if u < 0 || u > 1 then
            False

        else
            let
                qvec =
                    Vector3.cross tvec v0v1

                v =
                    invDet * Vector3.dot direction qvec

                -- If v is not between 0 and 1, the intersection is outside the triangle
            in
            if v < 0 || u + v > 1 then
                False

            else
                let
                    t =
                        invDet * Vector3.dot v0v2 qvec
                in
                -- If t is too large, the intersection is further away than the maximum
                -- distance the ray can travel
                -- NOTE: I removed the behind the origin code here!
                if t < 1000000.0 then
                    True

                else
                    False
