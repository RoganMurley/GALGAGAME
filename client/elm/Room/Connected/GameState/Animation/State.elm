module Animation.State exposing (..)

import Animation.Types exposing (Anim(..))
import Ease


animShake : Maybe Anim -> Float -> Float
animShake anim tick =
    let
        baseMag =
            case anim of
                Just (Slash _ d) ->
                    5.0 * Ease.outQuad (toFloat d / 50.0)

                Just (Bite _ d) ->
                    5.0 * Ease.outQuad (toFloat d / 50.0)

                Just (Obliterate _) ->
                    20.0

                Just (Play _ _ _) ->
                    1.0

                _ ->
                    0.0

        mag =
            baseMag * (1.0 - Ease.outQuad (tick / animMaxTick anim))
    in
        mag * 0.03 * (toFloat <| (ceiling tick * 1247823748932 + 142131) % 20) - 10


animMaxTick : Maybe Anim -> Float
animMaxTick anim =
    case anim of
        Just (Draw _) ->
            500.0

        Just (Reverse _) ->
            1500.0

        Just (Play _ _ _) ->
            500.0

        Just (Overdraw _ _) ->
            1000.0

        Just (Obliterate _) ->
            1000.0

        Just (GameEnd _) ->
            2500.0

        Just (Rotate _) ->
            1000.0

        Just (Windup _) ->
            300.0

        _ ->
            800.0


progress : Maybe Anim -> Float -> Float
progress anim tick =
    let
        maxTick : Float
        maxTick =
            animMaxTick anim

        easingFunction : Float -> Float
        easingFunction =
            case anim of
                Just (Heal _) ->
                    Ease.outQuad

                Just (Overdraw _ _) ->
                    Ease.outQuint

                Just (Rotate _) ->
                    Ease.inQuad

                Just (Slash _ _) ->
                    Ease.outQuad

                Just (Windup _) ->
                    Ease.inQuad

                _ ->
                    Ease.outQuint
    in
        easingFunction (tick / maxTick)
