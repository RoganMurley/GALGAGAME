module Messages exposing (GameMsg(..), MenuMsg(..), Msg(..))

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
    | PlayCard String
    | NewChatMsg String
    | GameStateMsg GameMsg
    | ConnectError String
    | Spectate
    | Play
    | KeyPress Int
    | Rematch
    | Tick Time
    | ResolveStep
    | HoverCard (Maybe String)
    | MainMenuMsg MenuMsg
    | SelectAllInput String


type GameMsg
    = Sync String
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String


type MenuMsg
    = MenuCustom
