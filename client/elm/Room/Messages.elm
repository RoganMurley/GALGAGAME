module Room.Messages exposing (Msg(..))

import Clock.Messages as Clock
import Connected.Messages as Connected
import Lab.Messages as Lab
import Lobby.Messages as Lobby
import Login.Messages as Login
import Menu.Messages as Menu
import Mode exposing (Mode)
import Replay.Messages as Replay


type Msg
    = ClockMsg Clock.Msg
    | ConnectedMsg Connected.Msg
    | LabMsg Lab.Msg
    | LobbyMsg Lobby.Msg
    | LoginMsg Login.Msg
    | MenuMsg Menu.Msg
    | ReplayMsg Replay.Msg
    | StartGame Mode
