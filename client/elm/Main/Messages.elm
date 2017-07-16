module Main.Messages exposing (Msg(..))

import Chat.Messages as Chat
import Drag.Messages as Drag
import GameState.Messages as GameState
import Lobby.Messages as Lobby
import Main.Types exposing (Mode)
import Menu.Messages as Menu
import Navigation
import Time exposing (Time)


type Msg
    = UrlChange Navigation.Location
    | Send String
    | Receive String
    | DrawCard
    | EndTurn
    | PlayCard Int
    | StartGame Mode
    | KeyPress Int
    | Rematch
    | Tick Time
    | Frame Float
    | Resize Int Int
    | ResolveStep
    | HoverCard (Maybe Int)
    | SelectAllInput String
    | CopyInput String
    | ChatMsg Chat.Msg
    | DragMsg Drag.Msg
    | GameStateMsg GameState.Msg
    | LobbyMsg Lobby.Msg
    | MenuMsg Menu.Msg
    | PlayingOnly Msg
