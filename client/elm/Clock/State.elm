module Clock.State exposing (..)

import Animation.Types exposing (Anim(..))
import Clock.Types exposing (Model, Uniforms)
import Math.Matrix4 exposing (Mat4, identity, makeLookAt, makeOrtho, makeRotate, mul)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Model.State as Model
import Raymarch.Types exposing (Height, Width)
import WebGL exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(PlayerA))
import Resolvable.State as Resolvable


init : Model
init =
    let
        mInit =
            Model.init

        card =
            { name = "", desc = "", imgURL = "" }

        stackCard =
            { owner = PlayerA, card = card }

        stackLen =
            11

        model =
            { mInit
                | hand = List.repeat 5 card
                , otherHand = 6
                , stack = List.repeat stackLen stackCard
            }
    in
        { res =
            Resolvable.init model <|
                List.concat
                    [ [ { model = { model | hand = List.drop 1 model.hand }
                        , anim = Just (GameStart PlayerA)
                        , stackCard = Nothing
                        }
                      ]
                    , [ { model = model
                        , anim = Just (Draw PlayerA)
                        , stackCard = Nothing
                        }
                      ]
                    , (List.map
                        (\i ->
                            { model = { model | stack = List.drop i model.stack }
                            , anim = Just (Rotate PlayerA)
                            , stackCard = Nothing
                            }
                        )
                        (List.range 0 stackLen)
                      )
                    ]
        }


tick : Model -> Float -> Model
tick ({ res } as model) dt =
    let
        newRes =
            Resolvable.tick dt res
    in
        { model | res = newRes }


uniforms : Float -> ( Width, Height ) -> Texture -> Vec3 -> Mat4 -> Mat4 -> Uniforms {}
uniforms t ( width, height ) texture pos rot scale =
    { resolution = vec2 (toFloat width) (toFloat height)
    , texture = texture
    , rotation = rot
    , scale = scale
    , worldPos = pos
    , worldRot = makeRotate 0 (vec3 0 0 1)
    , perspective = makeOrtho 0 (toFloat width / 2) (toFloat height / 2) 0 0.01 1000
    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
    }


clockFace : Int -> Vec3 -> Float -> Float -> List ( Vec3, Mat4 )
clockFace n origin radius progress =
    let
        segments : Int
        segments =
            12

        indexes : List Int
        indexes =
            List.range 1 n

        genPoint : Int -> ( Vec3, Mat4 )
        genPoint i =
            ( Math.Vector3.add origin (offset i), rotation i )

        segmentAngle : Float
        segmentAngle =
            -2.0 * pi / (toFloat segments)

        rot : Int -> Float
        rot i =
            (toFloat i)
                * segmentAngle
                - progress
                * segmentAngle

        offset : Int -> Vec3
        offset i =
            Math.Vector3.scale -radius <|
                vec3 (sin (rot i)) (cos (rot i)) 0

        rotation : Int -> Mat4
        rotation i =
            makeRotate (2 * pi - (rot i)) (vec3 0 0 1)
    in
        List.map genPoint indexes
