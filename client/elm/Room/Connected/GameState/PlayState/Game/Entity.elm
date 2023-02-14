module Game.Entity exposing (Entity, Entity3D, toTriangles)

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Quaternion exposing (Quaternion)


type alias Entity a =
    { a
        | position : Vec2
        , rotation : Float
        , scale : Float
    }


type alias Entity3D a =
    { a
        | position : Vec3
        , rotation : Quaternion
        , scale : Vec3
    }


toTriangles : Entity3D a -> List ( Vec3, Vec3, Vec3 )
toTriangles { position, rotation, scale } =
    let
        transform : Mat4
        transform =
            Matrix4.identity
                |> Matrix4.scale scale
                |> Matrix4.mul (Quaternion.makeRotate rotation)
                |> Matrix4.translate position
    in
    [ ( Matrix4.transform transform <| vec3 -1 1 0
      , Matrix4.transform transform <| vec3 1 1 0
      , Matrix4.transform transform <| vec3 -1 -1 0
      )
    ]
