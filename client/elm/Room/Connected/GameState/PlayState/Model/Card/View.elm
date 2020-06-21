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

        glyphColour =
            case owner of
                PlayerA ->
                    Colour.white

                PlayerB ->
                    Colour.black

        orbTexturePath =
            case owner of
                PlayerA ->
                    "cardOrb.png"

                PlayerB ->
                    "cardOrbOther.png"
    in
    Texture.with3 textures card.imgURL "cardBackBack.png" orbTexturePath <|
        \texture cardBackTexture cardOrbTexture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.cardCol card.col
                , pos = pos
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = cardBackTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.white
                , pos = pos
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = cardOrbTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = rot
                , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                , color = glyphColour
                , pos = pos
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = texture
                }
            ]


backView : Context -> Game.Entity {} -> List WebGL.Entity
backView { w, h, radius, textures } { position, rotation, scale } =
    let
        { width, height } =
            baseDimensions radius
    in
    Texture.with textures "cardBackBack.png" <|
        \texture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = makeRotate rotation <| vec3 0 0 1
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.white
                , pos = to3d position
                , worldRot = makeRotate 0 <| vec3 0 0 1
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , texture = texture
                }
            ]


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

        glyphColour =
            case owner of
                PlayerA ->
                    Colour.white

                PlayerB ->
                    Colour.black

        progress =
            case anim of
                Limbo _ ->
                    1 - ctx.progress

                Unlimbo _ ->
                    ctx.progress

                _ ->
                    0

        orbTexturePath =
            case owner of
                PlayerA ->
                    "cardOrb.png"

                PlayerB ->
                    "cardOrbOther.png"
    in
    Texture.with3 textures card.imgURL orbTexturePath "cardBackBack.png" <|
        \texture cardOrbTexture cardBackTexture ->
            [ Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = cardBackTexture
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.cardCol card.col
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , alpha = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = cardOrbTexture
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.white
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , alpha = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = texture
                , rotation = rot
                , scale = makeScale3 (scale * 0.6 * width) (scale * 0.6 * height) 1
                , color = glyphColour
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , alpha = progress
                }
            ]


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

        glyphColour =
            case owner of
                PlayerA ->
                    Colour.white

                PlayerB ->
                    Colour.black

        orbTexturePath =
            case owner of
                PlayerA ->
                    "cardOrb.png"

                PlayerB ->
                    "cardOrbOther.png"
    in
    Texture.with4 textures card.imgURL orbTexturePath "cardBackBack.png" "noise.png" <|
        \texture cardOrbTexture cardBackTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardBackTexture
                , noise = noise
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.cardCol card.col
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardOrbTexture
                , noise = noise
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.white
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
                , color = glyphColour
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , time = progress
                }
            ]


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

        glyphColour =
            case owner of
                PlayerA ->
                    Colour.white

                PlayerB ->
                    Colour.black

        orbTexturePath =
            case owner of
                PlayerA ->
                    "cardOrb.png"

                PlayerB ->
                    "cardOrbOther.png"
    in
    Texture.with4 textures card.imgURL orbTexturePath "cardBackBack.png" "noise.png" <|
        \texture cardOrbTexture cardBackTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardBackTexture
                , noise = noise
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.cardCol card.col
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , time = 1 - progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardOrbTexture
                , noise = noise
                , rotation = rot
                , scale = makeScale3 (scale * width) (scale * height) 1
                , color = Colour.white
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
                , color = glyphColour
                , pos = pos
                , worldRot = makeRotate 0 (vec3 0 0 1)
                , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                , time = 1 - progress
                }
            ]


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

        ( mCardBack, glyphColour ) =
            case stackCard.owner of
                PlayerA ->
                    ( Texture.load textures "cardBack.png", Colour.white )

                PlayerB ->
                    ( Texture.load textures "cardBackRed.png", Colour.black )

        ( mFinalCardBack, finalGlyphColour ) =
            case finalStackCard.owner of
                PlayerA ->
                    ( Texture.load textures "cardBack.png", Colour.white )

                PlayerB ->
                    ( Texture.load textures "cardBackRed.png", Colour.black )
    in
    case ( mTexture, mFinalTexture ) of
        ( Just texture, Just finalTexture ) ->
            case ( mCardBack, mFinalCardBack ) of
                ( Just cardBackTexture, Just finalCardBackTexture ) ->
                    [ Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                        { texture = cardBackTexture
                        , finalTexture = finalCardBackTexture
                        , rotation = rot
                        , scale = makeScale3 (scale * width) (scale * height) 1
                        , color = Colour.white
                        , finalColor = Colour.white
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
                        , color = glyphColour
                        , finalColor = finalGlyphColour
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
