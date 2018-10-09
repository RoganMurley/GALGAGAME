module Card.View exposing (..)

import Card.State exposing (cardTexture)
import Card.Types as Card
import Clock.Entity exposing (GameEntity)
import Clock.Primitives
import Clock.Shaders
import Clock.Types exposing (Context)
import Colour
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Util exposing (to3d)
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


baseDimensions : Float -> { width : Float, height : Float }
baseDimensions radius =
    { width = 0.1 * radius
    , height = 0.1 * radius
    }


view : Context -> Card.Entity a -> List WebGL.Entity
view { w, h, radius, textures } { position, rotation, scale, card, owner } =
    let
        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            Colour.card owner
    in
        case cardTexture textures card of
            Just texture ->
                [ Clock.Primitives.roundedBox
                    { rotation = rot
                    , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
                    , color = col
                    , worldPos = pos
                    , worldRot = makeRotate 0 <| vec3 0 0 1
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    }
                , Clock.Primitives.quad Clock.Shaders.fragment <|
                    { rotation = rot
                    , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                    , color = col
                    , worldPos = pos
                    , worldRot = makeRotate 0 <| vec3 0 0 1
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , texture = texture
                    }
                ]

            Nothing ->
                []


backView : Context -> GameEntity {} -> WebGL.Entity
backView { w, h, radius } { position, rotation, scale } =
    let
        { width, height } =
            baseDimensions radius
    in
        Clock.Primitives.roundedBox <|
            { rotation = makeRotate rotation <| vec3 0 0 1
            , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
            , color = Colour.card PlayerB
            , worldPos = to3d position
            , worldRot = makeRotate 0 <| vec3 0 0 1
            , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
            , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
            }


dissolvingView : Context -> Card.Entity a -> List WebGL.Entity
dissolvingView ctx { position, rotation, scale, card, owner } =
    let
        { w, h, radius, progress, textures } =
            ctx

        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            Colour.card owner

        mTexture =
            cardTexture textures card

        mNoise =
            Texture.load textures "noise"
    in
        case ( mTexture, mNoise ) of
            ( Just texture, Just noise ) ->
                [ Clock.Primitives.roundedBoxDisintegrate <|
                    { texture = noise
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
                    { texture = texture
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


transmutingView : Context -> StackCard -> StackCard -> Card.Entity a -> List WebGL.Entity
transmutingView ctx stackCard finalStackCard { position, rotation, scale } =
    let
        { w, h, radius, progress, textures } =
            ctx

        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        col =
            Colour.card stackCard.owner

        mTexture =
            cardTexture textures stackCard.card

        mFinalTexture =
            cardTexture textures finalStackCard.card
    in
        case ( mTexture, mFinalTexture ) of
            ( Just texture, Just finalTexture ) ->
                [ Clock.Primitives.roundedBoxTransmute <|
                    { rotation = rot
                    , scale = makeScale3 (scale * 0.7 * width) (scale * height) 1
                    , color = col
                    , finalColor = Colour.card finalStackCard.owner
                    , worldPos = pos
                    , worldRot = makeRotate 0 (vec3 0 0 1)
                    , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                    , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                    , time = progress
                    }
                , Clock.Primitives.quad Clock.Shaders.fragmentTransmute <|
                    { texture = texture
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
