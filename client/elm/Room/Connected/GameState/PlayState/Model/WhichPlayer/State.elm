module WhichPlayer.State exposing (..)

import WhichPlayer.Types exposing (WhichPlayer(..))


other : WhichPlayer -> WhichPlayer
other which =
    case which of
        PlayerA ->
            PlayerB

        PlayerB ->
            PlayerA
