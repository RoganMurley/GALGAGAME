module Waiting.State exposing (init, tick)

import Waiting.Types exposing (Model, WaitType(..))


init : WaitType -> Model
init waitType =
    { waitType = waitType
    , bounceTick = 0
    }


tick : Float -> Model -> Model
tick dt model =
    { model | bounceTick = model.bounceTick + dt }
