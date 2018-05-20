module Clock.Listener exposing (..)

import Animation.Types exposing (Anim(Rotate))
import Audio exposing (playSound)
import Clock.Types exposing (Model)
import Main.Messages exposing (Msg)
import Resolvable.State exposing (activeAnim)


listen : Model -> Cmd Msg
listen { res } =
    case activeAnim res of
        Just (Rotate _) ->
            if res.tick == 0 then
                playSound "/sfx/tick.mp3"
            else
                Cmd.none

        otherwise ->
            Cmd.none
