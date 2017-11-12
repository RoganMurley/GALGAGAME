module Lab.State exposing (..)

import Lab.Messages exposing (Msg(..))
import Lab.Types exposing (Model)
import Card.Types exposing (Anim(..))
import Model.Types exposing (WhichPlayer(..))
import GameState.State exposing (tickZero)


init : Model
init =
    { player = PlayerA
    , anim = Heal
    , time = 0.0
    }


update : Model -> Msg -> Model
update model msg =
    case msg of
        SetPlayer player ->
            { model | player = player }

        SetAnim anim ->
            { model | anim = anim }


tickForward : Model -> Float -> Model
tickForward model dt =
    let
        newTime : Float
        newTime =
            if tickZero model.time then
                0.0
            else
                model.time + dt
    in
        { model | time = newTime }
