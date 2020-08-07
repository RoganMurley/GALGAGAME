module Background.View exposing (radialView, webglView)

import Animation.Types exposing (Anim(..))
import Assets.Types as Assets
import Colour
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Render.Types as Render
import Texture.State as Texture
import Texture.Types as Texture
import Vfx.State as Vfx
import Vfx.Types as Vfx
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


webglView : Render.Params -> Assets.Model -> Anim -> List WebGL.Entity
webglView { w, h, time } assets anim =
    let
        baseCtx =
            bareContextInit ( w, h ) assets Nothing

        ctx =
            { baseCtx | anim = anim, progress = time / 4000 }
    in
    radialView Vfx.init ctx


getRingRotation : Context -> Float
getRingRotation { anim, model, progress } =
    let
        rot =
            case anim of
                Rotate _ ->
                    toFloat model.rot - 1 + progress

                Windup _ ->
                    toFloat model.rot + (1 - progress)

                Finding ->
                    -12 * progress

                _ ->
                    toFloat model.rot
    in
    rot * -2.0 * pi / 12.0


radialView : Vfx.Model -> Context -> List WebGL.Entity
radialView { rotation } ({ camera2d, ortho, w, h, textures } as ctx) =
    let
        size =
            1.4 * max w h
    in
    Texture.with textures "radial.png" <|
        \texture ->
            [ Render.Primitives.quad Render.Shaders.tunnel
                { rotation = makeRotate pi (vec3 0 0 1)
                , scale = makeScale3 (0.5 * size) (0.5 * size) 1
                , color = Colour.white
                , pos = vec3 (w * 0.5) (h * 0.5) 0
                , perspective = ortho
                , camera = camera2d
                , spin = getRingRotation ctx
                , depth = rotation
                , texture = texture
                }
            ]
