module Clock.Card exposing (..)

import Card.Types exposing (Card)
import Clock.Primitives
import Clock.Shaders
import Clock.State exposing (uniforms)
import Clock.Types exposing (ClockParams, GameEntity)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (Vec3, vec3)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (to3d)


type alias CardEntity =
    GameEntity
        { card : Card
        , owner : WhichPlayer
        }


cardEntity : ClockParams -> Texture.Model -> CardEntity -> List WebGL.Entity
cardEntity { w, h, radius } textures { position, rotation, scale, card, owner } =
    let
        ( width, height, spacing ) =
            ( 0.1 * radius, 0.1 * radius, 35.0 )

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            colour owner

        mTexture =
            cardTexture textures card
    in
        case mTexture of
            Just texture ->
                [ Clock.Primitives.roundedBox <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (scale * 0.7 * width) (scale * height) 1)
                        col
                , Clock.Primitives.quad Clock.Shaders.fragment <|
                    uniforms 0
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1)
                        col
                ]

            Nothing ->
                []


cardTexture : Texture.Model -> Card -> Maybe Texture
cardTexture textures { imgURL } =
    Texture.load textures imgURL


colour : WhichPlayer -> Vec3
colour which =
    case which of
        PlayerA ->
            vec3 0.52 0.1 0.2

        PlayerB ->
            vec3 0.18 0.49 0.62
