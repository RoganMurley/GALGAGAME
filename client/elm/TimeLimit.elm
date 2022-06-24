module TimeLimit exposing (progress)

import Ease


progress : Float -> Float -> Float
progress warningSeconds t =
    Ease.inOutSine
        (1 - (clamp 0 (warningSeconds * 1000) t / (warningSeconds * 1000)))
