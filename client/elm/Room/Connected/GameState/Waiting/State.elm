module Waiting.State exposing (init, mouseDown, tick)

import Assets.Types as Assets
import Game.State exposing (bareContextInit)
import Main.Types exposing (Flags)
import Mouse exposing (MouseState(..), Position)
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


mouseDown : Flags -> Assets.Model -> Position -> Model -> Model
mouseDown flags assets _ model =
    let
        ctx =
            bareContextInit flags.dimensions assets NoMouse
    in
    { model | bulge = model.bulge + ctx.radius * 0.05 }
