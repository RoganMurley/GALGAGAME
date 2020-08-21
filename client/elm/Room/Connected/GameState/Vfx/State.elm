module Vfx.State exposing (init, tick)

import Ease
import Game.Types exposing (Context)
import Vfx.Types exposing (Model)


init : Model
init =
    { rotation = 0, brightness = -1.0 }


tick : Float -> Model -> Context -> Model
tick dt model ctx =
    let
        risk =
            (toFloat <| List.length ctx.model.stack)
                / 12

        targetBrightness =
            -0.8 + 2 * Ease.inSine risk

        brightnessDist =
            model.brightness - targetBrightness

        brightnessSpeed =
            if brightnessDist > 0 then
                0.0001

            else
                0.007

        brightness =
            if abs brightnessDist > 0.01 then
                model.brightness - brightnessSpeed * brightnessDist * dt

            else
                model.brightness
    in
    { model
        | rotation = model.rotation + 10 + 100 * dt * Ease.inSine risk
        , brightness = brightness
    }
