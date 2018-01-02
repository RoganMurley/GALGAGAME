module Room.Types exposing (..)

import Connected.Types as Connected
import Lab.Types as Lab
import Lobby.Types as Lobby
import Login.Types as Login


type Model
    = MainMenu
    | Lobby Lobby.Model
    | Connected Connected.Model
    | Lab Lab.Model
    | Login Login.Model
