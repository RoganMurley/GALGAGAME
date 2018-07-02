module Clock.Stack exposing (..)

import Clock.Primitives as Primitives
import Clock.Shaders
import Clock.State exposing (animToResTickMax, clockFace, uniforms)
import Clock.Types exposing (ClockParams, GameEntity)
import Math.Matrix4 exposing (Mat4, makeRotate, makeScale3)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))


view : ClockParams -> List (GameEntity { owner : WhichPlayer }) -> Texture -> List WebGL.Entity
view { w, h, radius } entities texture =
    let
        to3d : Vec2 -> Vec3
        to3d pos =
            vec3 (Math.Vector2.getX pos) (Math.Vector2.getY pos) 0

        makeCard : GameEntity { owner : WhichPlayer } -> List WebGL.Entity
        makeCard { owner, position, rotation } =
            let
                pos =
                    to3d position

                rot =
                    makeRotate rotation (vec3 0 0 1)
            in
                [ Primitives.roundedBox <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (0.7 * 0.13 * radius) (0.13 * radius) 1)
                        (case owner of
                            PlayerA ->
                                vec3 0.18 0.49 0.62

                            PlayerB ->
                                vec3 0.52 0.1 0.2
                        )
                , Primitives.quad Clock.Shaders.fragment <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (0.13 * radius) (0.13 * radius) 1)
                        (vec3 1 1 1)
                ]
    in
        List.concat <| List.map makeCard entities
