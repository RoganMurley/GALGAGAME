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
        makeEntity i =
            case ctx.anim of
                Transmute transmutations ->
                    case Wheel.get i transmutations |> Maybe.join of
                        Just (Transmutation ca cb) ->
                            Card.transmutingView ctx ca cb

                        _ ->
                            Card.view ctx

                Bounce bounces ->
                    case Wheel.get i bounces |> Maybe.join of
                        Just (BounceIndex _ _) ->
                            \_ -> []

                        Just BounceDiscard ->
                            Card.dissolvingView ctx

                        _ ->
                            Card.view ctx

                DiscardStack discards ->
                    case Wheel.get i discards of
                        Just True ->
                            Card.dissolvingView ctx

                        _ ->
                            Card.view ctx

                _ ->
                    Card.view ctx
    in
    List.concat <| List.indexedMap makeEntity entities


wheelBgView : List WheelEntity -> Context -> List WebGL.Entity
wheelBgView entities { camera3d, perspective, textures } =
    let
        wheelEntityView : WheelEntity -> List WebGL.Entity
        wheelEntityView { position, scale, rotation } =
            Texture.with textures "cardOutline.png" <|
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
