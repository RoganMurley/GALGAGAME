module Stack.View exposing (view, wheelBgView)

import Animation.Types exposing (Anim(..), Bounce(..), CardDiscard(..), Transmutation(..))
import Card.View as Card
import Colour
import Game.Types exposing (Context, StackEntity, WheelEntity)
import Math.Matrix4 exposing (makeRotate, makeScale)
import Maybe.Extra as Maybe
import Quaternion
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL
import Wheel.State as Wheel


view : List StackEntity -> Context -> List WebGL.Entity
view entities ctx =
    let
        makeEntity entity =
            case ctx.anim of
                Transmute transmutations ->
                    case Wheel.get entity.index transmutations |> Maybe.join of
                        Just (Transmutation ca cb) ->
                            Card.transmutingView ctx ca cb entity

                        _ ->
                            Card.view ctx entity

                Bounce bounces ->
                    case Wheel.get entity.index bounces |> Maybe.join of
                        Just (BounceIndex _ _) ->
                            []

                        Just BounceDiscard ->
                            Card.dissolvingView ctx entity

                        _ ->
                            Card.view ctx entity

                DiscardStack discards ->
                    case Wheel.get entity.index discards of
                        Just True ->
                            Card.dissolvingView ctx entity

                        _ ->
                            Card.view ctx entity

                _ ->
                    Card.view ctx entity
    in
    List.concat <| List.map makeEntity entities


wheelBgView : List WheelEntity -> Context -> List WebGL.Entity
wheelBgView entities { camera3d, perspective, textures } =
    let
        wheelEntityView : WheelEntity -> List WebGL.Entity
        wheelEntityView { position, scale, rotation } =
            Texture.with textures "cardBackOutline.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.fragmentAlpha <|
                        { rotation = Quaternion.makeRotate rotation
                        , scale = makeScale scale
                        , color = Colour.white
                        , pos = position
                        , perspective = perspective
                        , camera = camera3d
                        , texture = texture
                        , alpha = 0.5
                        }
                    ]
    in
    List.concat <| List.map wheelEntityView entities
