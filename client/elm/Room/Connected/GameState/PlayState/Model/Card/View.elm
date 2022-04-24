module Card.View exposing (backDissolvingView, backView, dissolvingView, revealedView, transmutingView, view)

import Animation.Types exposing (Anim(..))
import Card.Types as Card
import Colour
import Game.Entity as Game
import Game.Types exposing (Context)
import Math.Matrix4 as Matrix4 exposing (makeScale)
import Math.Vector3 as Vector3 exposing (vec3)
import Quaternion
import Render.Primitives
import Render.Shaders
import Stack.Types exposing (StackCard)
import Status.State as Status
import Status.Types exposing (Status(..))
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
                ++ (List.concat <| List.indexedMap (statusView ctx entity) (Status.collapse card.statuses))


statusView : Context -> Card.Entity a -> Int -> Status -> List WebGL.Entity
statusView ctx entity i status =
    let
        { camera3d, perspective, textures } =
            ctx

        { position, rotation, scale, revealed } =
            entity

        index =
            if revealed then
                i + 1

            else
                i

        column =
            modBy 3 index

        row =
            index // 3

        horizontalOffset =
            0.5 * (1 - toFloat column)

        verticalOffset =
            0.85 - 0.5 * toFloat row

        offset =
            Matrix4.transform
                (Quaternion.makeRotate rotation)
                (vec3
                    (Vector3.getX scale * horizontalOffset)
                    (Vector3.getY scale * verticalOffset)
                    0
                )
    in
    case status of
        StatusBlighted ->
            Texture.with textures "blighted.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.fragment <|
                        { rotation = Quaternion.makeRotate rotation
                        , scale = makeScale <| Vector3.scale 0.27 scale
                        , color = vec3 1 1 1
                        , pos = Vector3.add position offset
                        , perspective = perspective
                        , camera = camera3d
                        , texture = texture
                        }
                    ]

        StatusEcho ->
            Texture.with textures "multiply.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.fragment <|
                        { rotation = Quaternion.makeRotate rotation
                        , scale = makeScale <| Vector3.scale 0.27 scale
                        , color = vec3 1 1 1
                        , pos = Vector3.add position offset
                        , perspective = perspective
                        , camera = camera3d
                        , texture = texture
                        }
                    ]


revealedView : Context -> Card.Entity { a | revealed : Bool } -> List WebGL.Entity
revealedView ctx entity =
    let
        { camera3d, perspective, textures } =
            ctx

        { position, rotation, scale, revealed } =
            entity
    in
    if revealed then
        Texture.with textures "revealed.png" <|
            \texture ->
                [ Render.Primitives.quad Render.Shaders.fragment <|
                    { rotation = Quaternion.makeRotate rotation
                    , scale = makeScale scale
                    , color = vec3 1 1 1
                    , pos = position
                    , perspective = perspective
                    , camera = camera3d
                    , texture = texture
                    }
                ]

    else
        []


backView : Context -> Game.Entity3D a -> List WebGL.Entity
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


transmutingView : Context -> StackCard -> StackCard -> Card.Entity a -> List WebGL.Entity
transmutingView ctx stackCard finalStackCard entity =
    let
        { position, rotation, scale } =
            entity

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
                ++ (List.concat <| List.indexedMap (statusView ctx entity) (Status.collapse stackCard.card.statuses))


backDissolvingView : Context -> Game.Entity3D a -> List WebGL.Entity
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
