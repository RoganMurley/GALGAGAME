module Main.Messages exposing (Msg(..))

import Drag.Messages as Drag
import GameState.Messages as GameState
import Menu.Messages as Menu
import Time exposing (Time)


type Msg
    = Input String
    | Send String
    | Receive String
    | DragMsg Drag.Msg
    | DrawCard
    | EndTurn
    | PlayCard Int
    | NewChatMsg String
    | GameStateMsg GameState.Msg
    | ConnectError String
    | Spectate
    | Play
    | KeyPress Int
    | Rematch
    | Tick Time
    | Frame Float
    | Resize Int Int
    | ResolveStep
    | HoverCard (Maybe Int)
    | MenuMsg Menu.Msg
    | SelectAllInput String
    | CopyInput String
    | SelectCharacter String
