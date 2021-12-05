module TimeLimit exposing (progress)

import Ease


warningSeconds : Float
warningSeconds =
    20


progress : Float -> Float
progress t =
    Ease.inOutSine
        (1 - (clamp 0 (warningSeconds * 1000) t / (warningSeconds * 1000)))
