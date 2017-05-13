module Main.Messages exposing (Msg(..))

import Chat.Messages as Chat
import Drag.Messages as Drag
import GameState.Messages as GameState
import Main.Types exposing (Mode)
import Menu.Messages as Menu
import Time exposing (Time)


type Msg
    = Input String
    | Send String
    | Receive String
    | DrawCard
    | EndTurn
    | PlayCard Int
    | ConnectError String
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
    | MenuMsg Menu.Msg
    | PlayingOnly Msg
