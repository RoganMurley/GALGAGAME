module Waiting.State exposing (init, mouseDown, tick)

import Assets.Types as Assets
import Game.State exposing (bareContextInit)
import Main.Messages exposing (Msg(..))
import Main.Types exposing (Flags)
import Mouse exposing (MouseState(..), Position)
import Util exposing (message)
import Waiting.Types exposing (Model, WaitType(..))


init : Maybe WaitType -> Model
init mWaitType =
    { waitType = mWaitType
    , bounceTick = 0
    , seed = Nothing
    , bulge = 0
    }


tick : Float -> Model -> ( Model, Cmd Msg )
tick dt model =
    let
        ( newWaitType, msg ) =
            case model.waitType of
                Just (WaitChallenge t elapsed) ->
                    if t > 60000 && not elapsed then
                        ( Just <| WaitChallenge (t + dt) True, message EndChallenge )

                    else
                        ( model.waitType, Cmd.none )

                _ ->
                    ( model.waitType, Cmd.none )

        newModel =
            { model
                | bounceTick = model.bounceTick + dt
                , bulge = model.bulge * 0.95
                , waitType = newWaitType
            }
    in
    ( newModel, msg )


mouseDown : Flags -> Assets.Model -> Position -> Model -> Model
mouseDown flags assets _ model =
    let
        ctx =
            bareContextInit flags.dimensions assets NoMouse
    in
    { model | bulge = model.bulge + ctx.radius * 0.05 }
