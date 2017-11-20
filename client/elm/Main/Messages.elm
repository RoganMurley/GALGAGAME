module Main.Messages exposing (Msg(..))

import Connected.Types exposing (Mode)
import Drag.Messages as Drag
import Settings.Messages as Settings
import GameState.Messages as GameState
import Lobby.Messages as Lobby
import Lab.Messages as Lab
import Menu.Messages as Menu
import Navigation


type Msg
    = UrlChange Navigation.Location
    | Send String
    | Receive String
    | DrawCard
    | EndTurn
    | PlayCard Int
    | StartGame Mode
    | Rematch
    | Frame Float
    | Resize Int Int
    | HoverCard (Maybe Int)
    | SelectAllInput String
    | CopyInput String
    | DragMsg Drag.Msg
    | SettingsMsg Settings.Msg
    | GameStateMsg GameState.Msg
    | LobbyMsg Lobby.Msg
    | MenuMsg Menu.Msg
    | LabMsg Lab.Msg
    | PlayingOnly Msg
    | Concede
    | SetVolume Int
