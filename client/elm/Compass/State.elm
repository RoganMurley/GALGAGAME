module Compass.State exposing (..)

import Compass.Types exposing (Destination(..))
import Lobby.Types exposing (GameType(..))
import Navigation


url : Destination -> String
url dest =
    case dest of
        Root ->
            "/"

        Play gameType ->
            case gameType of
                CustomGame ->
                    "play/custom"

                ComputerGame ->
                    "play/cpu"


goto : Destination -> Cmd msg
goto dest =
    Navigation.newUrl <| url dest
