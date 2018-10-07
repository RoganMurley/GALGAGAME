module Clock.Card exposing (..)

import Card.Types exposing (Card)
import Clock.Primitives
import Clock.Shaders
import Clock.Uniforms exposing (uniforms)
import Clock.Types exposing (ClockParams, GameEntity)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import WebGL
import WebGL.Texture exposing (Texture)
import WhichPlayer.Types exposing (WhichPlayer(..))
import Util exposing (to3d)


type alias CardEntity a =
    GameEntity
        { a
            | card : Card
            , owner : WhichPlayer
        }


cardEntity : ClockParams -> Texture.Model -> CardEntity a -> List WebGL.Entity
cardEntity { w, h, radius } textures { position, rotation, scale, card, owner } =
    let
        ( width, height ) =
            ( 0.1 * radius, 0.1 * radius )

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
                [ Clock.Primitives.roundedBox
                    { resolution = vec2 w h
                    , rotation = rot
                    , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
                    , color = col
                    , worldPos = pos
                    , worldRot = makeRotate 0 <| vec3 0 0 1
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    }
                , Clock.Primitives.quad Clock.Shaders.fragment <|
                    uniforms
                        ( floor w, floor h )
                        texture
                        pos
                        rot
                        (makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1)
                        col
                ]

            Nothing ->
                []


cardBackEntity : ClockParams -> GameEntity {} -> WebGL.Entity
cardBackEntity { w, h, radius } { position, rotation, scale } =
    let
        ( width, height ) =
            ( 0.1 * radius, 0.1 * radius )
    in
        Clock.Primitives.roundedBox <|
            { resolution = vec2 w h
            , rotation = makeRotate rotation <| vec3 0 0 1
            , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
            , color = colour PlayerB
            , worldPos = to3d position
            , worldRot = makeRotate 0 <| vec3 0 0 1
            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
            }


dissolvingCardEntity : ClockParams -> Texture.Model -> Float -> CardEntity a -> List WebGL.Entity
dissolvingCardEntity { w, h, radius } textures progress { position, rotation, scale, card, owner } =
    let
        ( width, height ) =
            ( 0.1 * radius, 0.1 * radius )

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            colour owner

        mTexture =
            cardTexture textures card

        mNoise =
            Texture.load textures "noise"
    in
        case ( mTexture, mNoise ) of
            ( Just texture, Just noise ) ->
                [ Clock.Primitives.roundedBoxDisintegrate <|
                    { resolution = vec2 w h
                    , texture = noise
                    , rotation = rot
                    , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
                    , color = col
                    , worldPos = pos
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , time = progress
                    }
                , Clock.Primitives.quad Clock.Shaders.disintegrate <|
                    { resolution = vec2 w h
                    , texture = texture
                    , noise = noise
                    , rotation = rot
                    , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                    , color = vec3 1 1 1
                    , worldPos = pos
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , time = progress
                    }
                ]

            _ ->
                []


transmutingCardEntity : ClockParams -> Texture.Model -> Float -> StackCard -> StackCard -> CardEntity a -> List WebGL.Entity
transmutingCardEntity { w, h, radius } textures progress stackCard finalStackCard { position, rotation, scale } =
    let
        ( width, height ) =
            ( 0.1 * radius, 0.1 * radius )

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            colour stackCard.owner

        mTexture =
            cardTexture textures stackCard.card

        mFinalTexture =
            cardTexture textures finalStackCard.card
    in
        case ( mTexture, mFinalTexture ) of
            ( Just texture, Just finalTexture ) ->
                [ Clock.Primitives.roundedBoxTransmute <|
                    { resolution = vec2 w h
                    , rotation = rot
                    , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
                    , color = col
                    , finalColor = colour finalStackCard.owner
                    , worldPos = pos
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , time = progress
                    }
                , Clock.Primitives.quad Clock.Shaders.fragmentTransmute <|
                    { resolution = vec2 w h
                    , texture = texture
                    , finalTexture = finalTexture
                    , rotation = rot
                    , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                    , color = col
                    , worldPos = pos
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , time = progress
                    }
                ]

            _ ->
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
