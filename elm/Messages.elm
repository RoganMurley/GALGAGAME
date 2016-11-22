module Messages exposing (GameMsg(..), Msg(..))

import Mouse exposing (Position)


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


type GameMsg
    = Sync String
