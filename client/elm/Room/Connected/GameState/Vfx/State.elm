module Vfx.State exposing (init, tick)

import Ease
import Game.Types exposing (Context)
import Maybe.Extra as Maybe
import TimeLimit
import Vfx.Types exposing (Model)
import Wheel.State as Wheel


init : Model
init =
    { depth = 0
    , boogie = 0
    , tickle = Nothing
    }


tick : Float -> Model -> Maybe Float -> Context -> Model
tick dt model timeLeft ctx =
    let
        risk =
            ((toFloat <| List.length <| Maybe.values <| Wheel.toList ctx.model.stack) / 12)
                + timeLeftProgress

        timeLeftProgress =
            Maybe.withDefault 0 (Maybe.map (TimeLimit.progress 20) timeLeft)

        depth =
            model.depth + dt * 0.6 + 100 * dt * Ease.inSine risk

        boogie =
            model.boogie + dt + dt * Ease.inSine risk

        tickle =
            Maybe.andThen
                (\( index, t ) ->
                    if t - dt <= 0 then
                        Nothing

                    else
                        Just ( index, t - dt )
                )
                model.tickle
    in
    { model
        | depth = depth
        , boogie = boogie
        , tickle = tickle
    }
