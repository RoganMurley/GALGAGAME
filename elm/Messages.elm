module Messages exposing (GameMsg(..), Msg(..))

import Mouse exposing (Position)


type Msg
  = Input String
  | Send
  | Receive String
  | DragStart Position
  | DragAt Position
  | DragEnd Position
  | DrawCard
  | NewChatMsg String
  | GameStateMsg GameMsg

type GameMsg
  = Sync String
