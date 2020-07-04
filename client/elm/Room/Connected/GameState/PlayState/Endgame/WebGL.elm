module Endgame.WebGL exposing (animView, view)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Ease
import Font.View as Font
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeLookAt, makeOrtho, makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


animView : Context -> List WebGL.Entity
animView ({ w, h, radius, anim, progress } as ctx) =
    case anim of
        GameEnd winner ->
            let
                ( text, backgroundColor ) =
                    case winner of
                        Just PlayerA ->
                            ( "Victory", vec3 (30 / 255) (200 / 255) (30 / 255) )

                        Just PlayerB ->
                            ( "Defeat", vec3 (200 / 255) (30 / 255) (30 / 255) )

                        Nothing ->
                            ( "Draw", vec3 (255 / 255) (255 / 255) (255 / 255) )

                color =
                    vec3 (244 / 255) (241 / 255) (94 / 255)

                size =
                    radius * 3
            in
            List.concat
                [ [ Render.Primitives.quad Render.Shaders.matte
                        { rotation = makeRotate pi (vec3 0 0 1)
                        , scale = makeScale3 w h 1
                        , color = backgroundColor
                        , alpha = Ease.outCubic progress
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , worldRot = makeRotate 0 (vec3 0 0 1)
                        , perspective = makeOrtho 0 (w / 2) (h / 2) 0 0.01 1000
                        , camera = makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0)
                        }
                  ]
                , Font.view
                    "Rock Salt"
                    text
                    { x = w * 0.5 - 0.003 * size
                    , y = h * 0.4
                    , scaleX = 0.3 * Ease.outBounce (progress * progress)
                    , scaleY = 0.3 * Ease.outBounce (progress * progress)
                    , color = vec3 (40 / 255) (20 / 255) (20 / 255)
                    }
                    ctx
                , Font.view
                    "Rock Salt"
                    text
                    { x = w * 0.5
                    , y = h * 0.4
                    , scaleX = 0.3 * Ease.outBounce progress
                    , scaleY = 0.3 * Ease.outBounce progress
                    , color = color
                    }
                    ctx
                ]

        _ ->
            []


view : Render.Params -> Assets.Model -> Maybe WhichPlayer -> Bool -> List WebGL.Entity
view { w, h } assets winner resolving =
    let
        ctx =
            bareContextInit ( w, h ) assets Nothing
    in
    if resolving then
        []

    else
        animView { ctx | anim = GameEnd winner, progress = 1 }
