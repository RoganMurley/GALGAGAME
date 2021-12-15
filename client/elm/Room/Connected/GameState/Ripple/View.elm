module Ripple.View exposing (view)

import Colour
import Ease
import Game.Types exposing (Context)
import Render.Primitives
import Render.Uniforms exposing (uniColourMag)
import Ripple.Types exposing (Ripple)
import WebGL


view : List Ripple -> Context -> List WebGL.Entity
view ripples ctx =
    List.map
        (\{ progress, pos } ->
            let
                alpha =
                    Ease.inQuint (progress / 1000)

                scale =
                    ctx.radius * 0.0005 * (1000 - progress)
            in
            Render.Primitives.circle <|
                uniColourMag ctx
                    Colour.white
                    alpha
                    { scale = scale
                    , position = pos
                    , rotation = 0
                    }
        )
        ripples
