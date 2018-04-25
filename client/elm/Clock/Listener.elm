module Clock.Listener exposing (..)

import Audio exposing (playSound)
import Clock.Types exposing (Model)
import Main.Messages exposing (Msg)


listen : Model -> Cmd Msg
listen { time, turns } =
    if time == 0 && turns > 0 then
        playSound "/sfx/tick.mp3"
    else
        Cmd.none
