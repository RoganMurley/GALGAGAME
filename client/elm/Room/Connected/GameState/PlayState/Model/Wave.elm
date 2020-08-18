module Model.Wave exposing (view)

import Animation.Types exposing (Anim(..), Hurt(..))
import Colour exposing (Colour)
import Ease
import Game.Types exposing (Context)
import Math.Matrix4 exposing (makeRotate, makeScale3)
import Math.Vector3 exposing (vec3)
import Render.Primitives
import Render.Shaders
import Texture.State as Texture
import WebGL
import WhichPlayer.Types exposing (WhichPlayer(..))


view : Context -> List WebGL.Entity
view ctx =
    let
        { w, h, anim, progress, ortho, camera2d, textures } =
            ctx

        size =
            1.4 * max w h

        render : Int -> Float -> Colour -> List WebGL.Entity
        render damage time color =
            Texture.with textures "laser.png" <|
                \texture ->
                    [ Render.Primitives.quad Render.Shaders.laser
                        { rotation = makeRotate pi <| vec3 0 0 1
                        , scale = makeScale3 (0.5 * size) (0.5 * size) 1
                        , color = color
                        , pos = vec3 (w * 0.5) (h * 0.5) 0
                        , perspective = ortho
                        , camera = camera2d
                        , texture = texture
                        , progress = time
                        , hurt = Ease.outQuint <| toFloat damage / 50
                        }
                    ]
    in
    case anim of
        Hurt _ d _ ->
            render d progress <| vec3 1 0 0

        Heal _ m ->
            render m progress <| vec3 0 1 0

        _ ->
            []
