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
        transformMat : Mat4
        transformMat =
            Matrix4.scale scale <|
                Matrix4.mul (Quaternion.makeRotate rotation) <|
                    Matrix4.translate position <|
                        Matrix4.identity

        transformPoint : ( Vec3, Vec3, Vec3 ) -> ( Vec3, Vec3, Vec3 )
        transformPoint ( a, b, c ) =
            ( Matrix4.transform transformMat a
            , Matrix4.transform transformMat b
            , Matrix4.transform transformMat c
            )
    in
    [ transformPoint
        ( vec3 -1 1 0
        , vec3 1 1 0
        , vec3 -1 -1 0
        )
    , transformPoint
        ( vec3 -1 -1 0
        , vec3 1 1 0
        , vec3 1 -1 0
        )
    ]
