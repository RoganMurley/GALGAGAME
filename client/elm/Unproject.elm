module Unproject exposing (rayFromMouse, unproject)

import Collision exposing (Ray)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Maybe


rayFromMouse : Maybe Vec2 -> { w : Float, h : Float } -> Mat4 -> Mat4 -> Maybe Ray
rayFromMouse mMouse { w, h } perspective camera =
    mMouse
        |> Maybe.andThen
            (\mouse ->
                let
                    { x, y } =
                        Math.Vector2.toRecord mouse

                    coords =
                        { x = x / w
                        , y = y / h
                        }
                in
                unprojectRay coords perspective camera
            )


unprojectRay : { x : Float, y : Float } -> Mat4 -> Mat4 -> Maybe Ray
unprojectRay { x, y } perspective camera =
    Maybe.map2
        (\a b ->
            { direction = Vector3.sub a b |> Vector3.normalize
            , origin = vec3 0 0 -2
            }
        )
        (unproject { x = x, y = y, z = 0 } perspective camera)
        (unproject { x = x, y = y, z = -1 } perspective camera)


unproject : { x : Float, y : Float, z : Float } -> Mat4 -> Mat4 -> Maybe Vec3
unproject { x, y, z } perspective camera =
    let
        clipSpace : Vec3
        clipSpace =
            vec3 (x * 2 - 1) ((1 - y) * 2 - 1) z
    in
    Matrix4.inverse (Matrix4.mul perspective camera)
        |> Maybe.map
            (\inverted ->
                Matrix4.transform
                    inverted
                    clipSpace
            )
