module Ripple.View exposing (view)

import Assets.Types as Assets
import Colour
import Ease
import Game.State exposing (bareContextInit)
import Game.Types exposing (Context)
import Mouse exposing (MouseState(..))
import Render.Primitives
import Render.Types as Render
import Render.Uniforms exposing (uniColourMag)
import Ripple.Types exposing (Ripple)
import WebGL


view : List Ripple -> Render.Params -> Assets.Model -> List WebGL.Entity
view ripples { w, h } assets =
    let
        ctx =
            bareContextInit ( w, h ) assets NoMouse
    in
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
