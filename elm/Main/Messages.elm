module Main.Messages exposing (MenuMsg(..), Msg(..))

import Drag.Messages as Drag
import GameState.Messages as GameState
import Main.Types exposing (GameType)
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
    | MainMenuMsg MenuMsg
    | SelectAllInput String
    | CopyInput String
    | SelectCharacter String


type MenuMsg
    = MenuStart GameType
