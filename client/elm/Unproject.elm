module Unproject exposing (intersect, unproject, unprojectedRay)

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Vector4 as Vector4 exposing (Vec4, vec4)
import Maybe
import Tuple exposing (pair)


unprojectedRay : { x : Float, y : Float } -> Mat4 -> Mat4 -> Maybe Vec3
unprojectedRay { x, y } perspective camera =
    case
        ( unproject { x = x, y = y, z = 0 } perspective camera
        , unproject { x = x, y = y, z = 1 } perspective camera
        )
    of
        ( Just a, Just b ) ->
            Just <| Vector3.sub a b

        _ ->
            Nothing


unproject : { x : Float, y : Float, z : Float } -> Mat4 -> Mat4 -> Maybe Vec3
unproject { x, y, z } perspective camera =
    Matrix4.inverse (Matrix4.mul perspective camera)
        |> Maybe.map
            (\inverted ->
                Matrix4.transform
                    inverted
                    (vec3 ((1 - x) * 2 - 1) (y * 2 - 1) z)
            )


radius =
    0.08


intersect : Vec3 -> { a | position : Vec3 } -> Float
intersect ray { position } =
    let
        origin =
            vec3 0 0 1

        oc =
            Vector3.sub position origin

        a =
            Vector3.dot ray ray

        b =
            2 * Vector3.dot oc ray

        c =
            Vector3.dot oc oc - radius * radius

        discriminant =
            b * b - 4 * a * c
    in
    discriminant
