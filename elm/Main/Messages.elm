module Main.Messages exposing (MenuMsg(..), Msg(..))

import GameState.Messages as GameState
import Mouse exposing (Position)
import Time exposing (Time)


type Msg
    = Input String
    | Send String
    | Receive String
    | DragStart Position
    | DragAt Position
    | DragEnd Position
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
    = MenuCustom
    | MenuComputer
