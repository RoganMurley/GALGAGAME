module Vfx.State exposing (init, tick)

import Ease
import Game.Types exposing (Context)
import Maybe.Extra as Maybe
import Vfx.Types exposing (Model)
import Wheel.State as Wheel


init : Model
init =
    { depth = 0 }


tick : Float -> Model -> Context -> Model
tick dt model ctx =
    let
        risk =
            (toFloat <| List.length <| Maybe.values <| Wheel.toList ctx.model.stack)
                / 12

        depth =
            model.depth + 10 + 100 * dt * Ease.inSine risk
    in
    { model | depth = depth }
