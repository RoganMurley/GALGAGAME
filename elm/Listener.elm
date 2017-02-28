module Listener exposing (..)

import Audio exposing (SoundOption(..), playSound, playSoundWith)
import GameState exposing (GameState(..))
import Messages exposing (Msg)


listen : Float -> GameState -> Cmd Msg
listen time state =
    case state of
        PlayingGame _ ( [], _ ) ->
            playSoundWith "music/background.mp3" [ Loop, Once ]

        PlayingGame m _ ->
            if GameState.tickZero state then
                let
                    volume : Float
                    volume =
                        0.5 + 0.1 * (toFloat (List.length m.stack))
                in
                    playSoundWith "sfx/resolve.wav" [ Volume volume ]
            else
                Cmd.none

        otherwise ->
            Cmd.none
