module Room.Types exposing (..)

import Connected.Types as Connected
import Lobby.Types as Lobby
import Login.Types as Login
import Replay.Types as Replay


type Model
    = MainMenu
    | Lobby Lobby.Model
    | Connected Connected.Model
    | Replay Replay.Model
    | Login Login.Model
