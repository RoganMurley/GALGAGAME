module Listener exposing (..)

import GameState exposing (GameState(..))
import Messages exposing (Msg)
import Ports exposing (playAudio)


listen : Float -> GameState -> Cmd Msg
listen time state =
    case state of
        PlayingGame _ _ ->
            playAudio ( "music/background.mp3", True, True )

        otherwise ->
            Cmd.none
