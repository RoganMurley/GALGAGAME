module Model.Wave exposing (view)

import Animation.Types exposing (Anim(..), Hurt(..))
import Colour exposing (Colour)
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL


view : Context -> List WebGL.Entity
view ctx =
    let
        { w, h, anim, progress, ortho, camera2d, textures } =
            ctx

        size =
            1.4 * max w h

        render : Colour -> List WebGL.Entity
        render color =
            Texture.with textures "shock.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.shock
                        { rotation = makeRotate pi <| vec3 0 0 1
                        , scale = makeScale3 (0.5 * size) (0.5 * size) 1
                        , color = color
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , texture = texture
                        , progress = progress
                        }
                    ]
    in
    case anim of
        Hurt _ _ _ ->
            render Colour.red

        Heal _ _ ->
            render Colour.green

        _ ->
            []
