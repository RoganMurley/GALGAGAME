module Clock.Listener exposing (..)

import Animation.Types exposing (Anim(..))
import Audio exposing (playSound)
import Clock.Types exposing (Model)
import Main.Messages exposing (Msg)
import Resolvable.State exposing (activeAnim)


listen : Model -> Cmd Msg
listen { res } =
    if res.tick == 0 then
        case activeAnim res of
            Just (Rotate _) ->
                playSound "/sfx/evil_tick.mp3"

            Just (Draw _) ->
                playSound "/sfx/draw.wav"

            Just (Play _ _ _) ->
                playSound "/sfx/tick.wav"

            _ ->
                Cmd.none
    else
        Cmd.none
