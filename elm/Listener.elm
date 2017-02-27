module Listener exposing (..)

import GameState exposing (GameState(..))
import Messages exposing (Msg)
import Ports exposing (playAudio)


listen : Float -> GameState -> Cmd Msg
listen time state =
    case state of
        PlayingGame _ ( [], _ ) ->
            playAudio ( "music/background.mp3", True, True, 1.0 )

        PlayingGame m _ ->
            if GameState.tickZero state then
                let
                    volume : Float
                    volume =
                        0.5 + 0.1 * (toFloat (List.length m.stack))
                in
                    playAudio ( "sfx/resolve.wav", False, False, volume )
            else
                Cmd.none

        otherwise ->
            Cmd.none
