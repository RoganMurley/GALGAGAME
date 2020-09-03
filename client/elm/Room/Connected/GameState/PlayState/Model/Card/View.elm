module Card.View exposing (backDissolvingView, backView, dissolvingView, fabricatingView, limboingView, transmutingView, view)

import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Colour
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale)
import Math.Vector3 as Vector3 exposing (vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Context -> Card.Entity a -> List WebGL.Entity
view ctx entity =
    let
        { camera3d, perspective, textures } =
            ctx

        { position, rotation, scale, card, owner } =
            entity

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
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.cardCol card.col
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = cardBackTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = cardOrbTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = glyphColour
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = texture
                }
            ]


backView : Context -> Game.Entity3D {} -> List WebGL.Entity
backView { camera3d, perspective, textures } { position, rotation, scale } =
    Texture.with2 textures "cardBackBack.png" "cardOrbOther.png" <|
        \texture cardOrbTexture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = vec3 (200 / 255) (200 / 255) (200 / 255)
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = texture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = cardOrbTexture
                }
            ]


limboingView : Context -> Card.Entity a -> List WebGL.Entity
limboingView ctx { position, rotation, scale, card, owner } =
    let
        { perspective, camera3d, anim, textures } =
            ctx

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
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.cardCol card.col
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , alpha = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = cardOrbTexture
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , alpha = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = texture
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = glyphColour
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , alpha = progress
                }
            ]


dissolvingView : Context -> Card.Entity a -> List WebGL.Entity
dissolvingView ctx { position, rotation, scale, card, owner } =
    let
        { perspective, camera3d, progress, textures } =
            ctx

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
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.cardCol card.col
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardOrbTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = texture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = glyphColour
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            ]


fabricatingView : Context -> Card.Entity a -> List WebGL.Entity
fabricatingView ctx { position, rotation, scale, card, owner } =
    let
        { perspective, camera3d, progress, textures } =
            ctx

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
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.cardCol card.col
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = 1 - progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardOrbTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = 1 - progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = texture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = glyphColour
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = 1 - progress
                }
            ]


transmutingView : Context -> StackCard -> StackCard -> Card.Entity a -> List WebGL.Entity
transmutingView ctx stackCard finalStackCard { position, rotation, scale } =
    let
        { perspective, camera3d, progress, textures } =
            ctx

        ( glyphColour, orbTexturePath ) =
            case stackCard.owner of
                PlayerA ->
                    ( Colour.white, "cardOrb.png" )

                PlayerB ->
                    ( Colour.black, "cardOrbOther.png" )

        ( finalGlyphColour, finalOrbTexturePath ) =
            case finalStackCard.owner of
                PlayerA ->
                    ( Colour.white, "cardOrb.png" )

                PlayerB ->
                    ( Colour.black, "cardOrbOther.png" )
    in
    Texture.with5 textures stackCard.card.imgURL finalStackCard.card.imgURL orbTexturePath finalOrbTexturePath "cardBackBack.png" <|
        \texture finalTexture cardOrbTexture finalCardOrbTexture cardBackTexture ->
            [ Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.cardCol stackCard.card.col
                , finalColor = Colour.cardCol finalStackCard.card.col
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = cardBackTexture
                , finalTexture = cardBackTexture
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , finalColor = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = cardOrbTexture
                , finalTexture = finalCardOrbTexture
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = glyphColour
                , finalColor = finalGlyphColour
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = texture
                , finalTexture = finalTexture
                , time = progress
                }
            ]


backDissolvingView : Context -> Game.Entity3D {} -> List WebGL.Entity
backDissolvingView { perspective, camera3d, progress, textures } { position, rotation, scale } =
    Texture.with3 textures "cardOrbOther.png" "cardBackBack.png" "noise.png" <|
        \cardOrbTexture cardBackTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardBackTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = vec3 (200 / 255) (200 / 255) (200 / 255)
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = cardOrbTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            ]



-- Texture.with2 textures "cardBackBack.png" "cardOrbOther.png" <|
--     \texture cardOrbTexture ->
--         [ Render.Primitives.quad Render.Shaders.fragment <|
--             { rotation = Quaternion.makeRotate rotation
--             , scale = makeScale scale
--             , color = vec3 (200 / 255) (200 / 255) (200 / 255)
--             , pos = position
--             , perspective = perspective
--             , camera = camera3d
--             , texture = texture
--             }
--         , Render.Primitives.quad Render.Shaders.fragment <|
--             { rotation = Quaternion.makeRotate rotation
--             , scale = makeScale scale
--             , color = Colour.white
--             , pos = position
--             , perspective = perspective
--             , camera = camera3d
--             , texture = cardOrbTexture
--             }
--         ]
