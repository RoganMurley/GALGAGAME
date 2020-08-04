module Unproject exposing (intersect, unproject, unprojectedRay)

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3, vec3)
import Math.Vector4 as Vector4 exposing (Vec4, vec4)
import Maybe
import Tuple exposing (pair)


unprojectedRay : { x : Float, y : Float } -> Mat4 -> Mat4 -> Maybe { origin : Vec3, direction : Vec3 }
unprojectedRay { x, y } perspective camera =
    case
        ( unproject { x = x, y = y, z = 0 } perspective camera
        , unproject { x = x, y = y, z = -1 } perspective camera
        )
    of
        ( Just a, Just b ) ->
            Just
                { direction = Vector3.sub a b |> Vector3.normalize
                , origin = vec3 0 0 -2
                }

        _ ->
            Nothing


unproject : { x : Float, y : Float, z : Float } -> Mat4 -> Mat4 -> Maybe Vec3
unproject { x, y, z } perspective camera =
    let
        clipSpace : Vec3
        clipSpace =
            vec3 (x * 2 - 1) ((1 - y) * 2 - 1) z

        eyeSpace : Maybe Vec3
        eyeSpace =
            Matrix4.inverse perspective
                |> Maybe.map
                    (\inverted ->
                        Matrix4.transform
                            inverted
                            clipSpace
                    )

        worldSpace : Maybe Vec3
        worldSpace =
            case eyeSpace of
                Just eye ->
                    Matrix4.inverse camera
                        |> Maybe.map
                            (\inverted ->
                                Matrix4.transform
                                    inverted
                                    eye
                            )

                Nothing ->
                    Nothing
    in
    worldSpace


radius =
    0.16


intersect : { origin : Vec3, direction : Vec3 } -> { a | position : Vec3 } -> Float
intersect { origin, direction } { position } =
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
    discriminant
