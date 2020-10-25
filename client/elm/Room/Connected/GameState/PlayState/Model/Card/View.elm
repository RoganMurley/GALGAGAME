module Card.View exposing (backDissolvingView, backView, dissolvingView, fabricatingView, limboingView, transmutingView, view)

import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Colour
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale)
import Math.Vector3 as Vector3
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
    in
    Texture.with2 textures "cardBack.png" "cardOutline.png" <|
        \backTexture outlineTexture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card owner
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = backTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = outlineTexture
                }
            ]
                ++ (Texture.with textures card.imgURL <|
                        \texture ->
                            [ Render.Primitives.quad Render.Shaders.fragment <|
                                { rotation = Quaternion.makeRotate rotation
                                , scale = makeScale <| Vector3.scale 0.6 scale
                                , color = Colour.white
                                , pos = position
                                , perspective = perspective
                                , camera = camera3d
                                , texture = texture
                                }
                            ]
                   )


backView : Context -> Game.Entity3D {} -> List WebGL.Entity
backView { camera3d, perspective, textures } { position, rotation, scale } =
    Texture.with2 textures "cardBack.png" "cardOutline.png" <|
        \backTexture outlineTexture ->
            [ Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.background PlayerB
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = backTexture
                }
            , Render.Primitives.quad Render.Shaders.fragment <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.white
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = outlineTexture
                }
            ]


limboingView : Context -> Card.Entity a -> List WebGL.Entity
limboingView ctx { position, rotation, scale, card, owner } =
    let
        { perspective, camera3d, anim, textures } =
            ctx

        progress =
            case anim of
                Limbo _ ->
                    1 - ctx.progress

                Unlimbo _ ->
                    ctx.progress

                _ ->
                    0
    in
    Texture.with3 textures card.imgURL "cardOutline.png" "cardBack.png" <|
        \texture outlineTexture backTexture ->
            [ Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = backTexture
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card owner
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , alpha = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                { texture = outlineTexture
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
                , color = Colour.white
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
    in
    Texture.with4 textures card.imgURL "cardBack.png" "cardOutline.png" "noise.png" <|
        \texture backTexture outlineTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = backTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card owner
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = outlineTexture
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
                , color = Colour.white
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
    in
    Texture.with4 textures card.imgURL "cardBack.png" "cardOutline.png" "noise.png" <|
        \texture backTexture outlineTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = backTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card owner
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = 1 - progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = outlineTexture
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
                , color = Colour.white
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
    in
    Texture.with4 textures stackCard.card.imgURL finalStackCard.card.imgURL "cardOutline.png" "cardBack.png" <|
        \texture finalTexture outlineTexture backTexture ->
            [ Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card stackCard.owner
                , finalColor = Colour.card finalStackCard.owner
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , texture = backTexture
                , finalTexture = backTexture
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
                , texture = outlineTexture
                , finalTexture = outlineTexture
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.fragmentTransmute <|
                { rotation = Quaternion.makeRotate rotation
                , scale = makeScale <| Vector3.scale 0.6 scale
                , color = Colour.white
                , finalColor = Colour.white
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
    Texture.with3 textures "cardOutline.png" "cardBack.png" "noise.png" <|
        \outlineTexture backTexture noise ->
            [ Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = backTexture
                , noise = noise
                , rotation = Quaternion.makeRotate rotation
                , scale = makeScale scale
                , color = Colour.card PlayerB
                , pos = position
                , perspective = perspective
                , camera = camera3d
                , time = progress
                }
            , Render.Primitives.quad Render.Shaders.disintegrate <|
                { texture = outlineTexture
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
