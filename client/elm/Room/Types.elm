module Room.Types exposing (..)

import Connected.Types as Connected
import Lab.Types as Lab
import Lobby.Types as Lobby


type Model
    = MainMenu
    | Lobby Lobby.Model
    | Connected Connected.Model
    | Lab Lab.Model
