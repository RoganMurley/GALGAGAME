module Quaternion exposing (Quaternion, angle, axis, identity, lerp, make, makeRotate, rotate, scale, xRotation, yRotation, zRotation)

-- Based on https://package.elm-lang.org/packages/nphollon/geo3d/2.1.1/Quaternion

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, normalize, vec3)


type alias Quaternion =
    { scalar : Float
    , vector : Vec3
    }


make : Float -> Float -> Float -> Float -> Quaternion
make scalar x y z =
    { scalar = scalar, vector = vec3 x y z }


angle : Quaternion -> Float
angle { scalar, vector } =
    let
        halfTurn =
            Math.Vector3.length vector / scalar

        s =
            asin (2 * halfTurn / (1 + halfTurn ^ 2))
    in
    if scalar == 0 then
        pi

    else if abs halfTurn < 1 then
        s

    else
        pi - s


axis : Quaternion -> Vec3
axis { vector } =
    if Math.Vector3.length vector == 0 then
        vec3 1 0 0

    else
        Math.Vector3.normalize vector


makeRotate : Quaternion -> Mat4
makeRotate quaternion =
    Math.Matrix4.makeRotate (angle quaternion) (axis quaternion)
        |> Math.Matrix4.rotate pi (vec3 0 1 0)


identity : Quaternion
identity =
    make 1 0 0 0


xRotation : Float -> Quaternion
xRotation rot =
    make (cos (0.5 * rot)) (sin (0.5 * rot)) 0 0


yRotation : Float -> Quaternion
yRotation rot =
    make (cos (0.5 * rot)) 0 (sin (0.5 * rot)) 0


zRotation : Float -> Quaternion
zRotation rot =
    make (cos (0.5 * rot)) 0 0 (sin (0.5 * rot))


add : Quaternion -> Quaternion -> Quaternion
add qa qb =
    { scalar = qa.scalar + qb.scalar
    , vector = Math.Vector3.add qa.vector qb.vector
    }


rotate : Quaternion -> Quaternion -> Quaternion
rotate qa qb =
    { scalar = qb.scalar * qa.scalar - Math.Vector3.dot qb.vector qa.vector
    , vector =
        Math.Vector3.scale qb.scalar qa.vector
            |> Math.Vector3.add (Math.Vector3.scale qa.scalar qb.vector)
            |> Math.Vector3.add (Math.Vector3.cross qa.vector qb.vector)
    }


scale : Float -> Quaternion -> Quaternion
scale scaling { scalar, vector } =
    { scalar = scalar * scaling
    , vector = Math.Vector3.scale scaling vector
    }


lerp : Float -> Quaternion -> Quaternion -> Quaternion
lerp progress qa qb =
    add (scale (1 - progress) qa) (scale progress qb)
