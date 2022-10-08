module Waiting.State exposing (init, mouseDown, tick)

import Mouse exposing (Position)
import Waiting.Types exposing (Model, WaitType(..))


init : WaitType -> Model
init waitType =
    { waitType = waitType
    , bounceTick = 0
    , seed = Nothing
    , bulge = 0
    }


tick : Float -> Model -> Model
tick dt model =
    { model
        | bounceTick = model.bounceTick + dt
        , bulge = model.bulge * 0.95
    }


mouseDown : Position -> Model -> Model
mouseDown mousePos model =
    { model | bulge = model.bulge + 30 }
