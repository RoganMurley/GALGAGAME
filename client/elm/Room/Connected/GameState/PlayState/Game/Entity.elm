module Game.Entity exposing (Entity, Entity3D, toTriangles)

import Math.Matrix4 as Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import Quaternion exposing (Quaternion)
import Render.Meshes


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
            Matrix4.mul (Quaternion.makeRotate rotation) <|
                Matrix4.makeScale scale

        transformPoint : ( Vec3, Vec3, Vec3 ) -> ( Vec3, Vec3, Vec3 )
        transformPoint ( a, b, c ) =
            -- Not sure why the translation didn't work inside the transform matrix?
            ( Vector3.add position <| Matrix4.transform transformMat a
            , Vector3.add position <| Matrix4.transform transformMat b
            , Vector3.add position <| Matrix4.transform transformMat c
            )
    in
    [ transformPoint
        ( Render.Meshes.topLeft.position
        , Render.Meshes.topRight.position
        , Render.Meshes.bottomLeft.position
        )
    , transformPoint
        ( Render.Meshes.bottomLeft.position
        , Render.Meshes.topRight.position
        , Render.Meshes.bottomRight.position
        )
    ]
