module Room.Types exposing (..)

import Clock.Types as Clock
import Connected.Types as Connected
import Lab.Types as Lab
import Lobby.Types as Lobby
import Login.Types as Login
import Replay.Types as Replay


type Model
    = MainMenu
    | Lobby Lobby.Model
    | Connected Connected.Model
    | Replay Replay.Model
    | Lab Lab.Model
    | Login Login.Model
    | Clock Clock.Model
