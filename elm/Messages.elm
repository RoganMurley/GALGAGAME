module Messages exposing (Msg(..))

import Mouse exposing (Position)


type Msg
  = Input String
  | Send
  | Receive String
  | DragStart Position
  | DragAt Position
  | DragEnd Position
  | IncCount
  | NewChatMsg String
