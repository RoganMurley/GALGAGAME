module Room.Messages exposing (Msg(..))

import Connected.Messages as Connected
import Lab.Messages as Lab
import Lobby.Messages as Lobby
import Menu.Messages as Menu
import Mode exposing (Mode)


type Msg
    = ConnectedMsg Connected.Msg
    | LabMsg Lab.Msg
    | LobbyMsg Lobby.Msg
    | MenuMsg Menu.Msg
    | StartGame Mode
