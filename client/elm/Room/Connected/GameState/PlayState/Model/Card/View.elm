module Card.View exposing (backView, baseDimensions, dissolvingView, fabricatingView, limboingView, transmutingView, view)

import Animation.Types exposing (Anim(..))
import Card.State exposing (cardTexture)
import Card.Types as Card
import Colour
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
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
view ctx entity =
    let
        { w, h, radius, textures } =
            ctx

        { position, rotation, scale, card, owner } =
            entity

        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position
    in
    case ( cardTexture textures card, Texture.load textures "cardBack.png" ) of
        ( Just texture, Just cardBackTexture ) ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.background owner
                , pos = pos
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = cardBackTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = rot
                , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                , color = Colour.white
                , pos = pos
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = texture
                }
            ]

        _ ->
            []


backView : Context -> Game.Entity {} -> List WebGL.Entity
backView { w, h, radius, textures } { position, rotation, scale } =
    let
        { width, height } =
            baseDimensions radius
    in
    case Texture.load textures "cardBack.png" of
        Just texture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = makeRotate rotation <| vec3 0 0 1
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.card PlayerB
                , pos = to3d position
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = texture
                }
            ]

        Nothing ->
            []


limboingView : Context -> Card.Entity a -> List WebGL.Entity
limboingView ctx { position, rotation, scale, card, owner } =
    let
        { w, h, anim, radius, textures } =
            ctx

        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        mTexture =
            cardTexture textures card

        mCardBack =
            Texture.load textures "cardBack.png"

        progress =
            case anim of
                Limbo _ ->
                    1 - ctx.progress

                Unlimbo _ ->
                    ctx.progress

                _ ->
                    0
    in
    case ( mTexture, mCardBack ) of
        ( Just texture, Just cardBackTexture ) ->
            [ Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = cardBackTexture
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.card owner
                , alpha = progress
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = texture
                , rotation = rot
                , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                , color = Colour.white
                , alpha = progress
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                }
            ]

        _ ->
            []


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

        mTexture =
            cardTexture textures card

        mCardBack =
            Texture.load textures "cardBack.png"

        mNoise =
            Texture.load textures "noise.png"
    in
    case ( mTexture, mCardBack ) of
        ( Just texture, Just cardBackTexture ) ->
            case mNoise of
                Just noise ->
                    [ Render.Primitives.quad Render.Shaders.disintegrate <|
                        { texture = cardBackTexture
                        , noise = noise
                        , rotation = rot
                        , scale = makeScale3 (scale * width) (scale * height) 1
                        , color = Colour.card owner
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = progress
                        }
                    , Render.Primitives.quad Render.Shaders.disintegrate <|
                        { texture = texture
                        , noise = noise
                        , rotation = rot
                        , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                        , color = Colour.white
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = progress
                        }
                    ]

                _ ->
                    []

        _ ->
            []


fabricatingView : Context -> Card.Entity a -> List WebGL.Entity
fabricatingView ctx { position, rotation, scale, card, owner } =
    let
        { w, h, radius, progress, textures } =
            ctx

        { width, height } =
            baseDimensions radius

        rot =
            makeRotate rotation <| vec3 0 0 1

        pos =
            to3d position

        mTexture =
            cardTexture textures card

        mCardBack =
            Texture.load textures "cardBack.png"

        mNoise =
            Texture.load textures "noise.png"
    in
    case ( mTexture, mCardBack ) of
        ( Just texture, Just cardBackTexture ) ->
            case mNoise of
                Just noise ->
                    [ Render.Primitives.quad Render.Shaders.disintegrate <|
                        { texture = cardBackTexture
                        , noise = noise
                        , rotation = rot
                        , scale = makeScale3 (scale * width) (scale * height) 1
                        , color = Colour.card owner
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = 1 - progress
                        }
                    , Render.Primitives.quad Render.Shaders.disintegrate <|
                        { texture = texture
                        , noise = noise
                        , rotation = rot
                        , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                        , color = Colour.white
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = 1 - progress
                        }
                    ]

                _ ->
                    []

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

        mTexture =
            cardTexture textures stackCard.card

        mFinalTexture =
            cardTexture textures finalStackCard.card

        mCardBack =
            Texture.load textures "cardBack.png"
    in
    case ( mTexture, mFinalTexture ) of
        ( Just texture, Just finalTexture ) ->
            case mCardBack of
                Just cardBackTexture ->
                    [ Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                        { texture = cardBackTexture
                        , finalTexture = cardBackTexture
                        , rotation = rot
                        , scale = makeScale3 (scale * width) (scale * height) 1
                        , color = Colour.card stackCard.owner
                        , finalColor = Colour.card finalStackCard.owner
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = progress
                        }
                    , Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                        { texture = texture
                        , finalTexture = finalTexture
                        , rotation = rot
                        , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                        , color = Colour.white
                        , finalColor = Colour.white
                        , pos = pos
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        , time = progress
                        }
                    ]

                Nothing ->
                    []

        _ ->
            []
