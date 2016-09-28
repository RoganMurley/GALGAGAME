module Messages exposing (Msg(..))

import Mouse exposing (Position)


type Msg
  = Input String
  | Send
  | NewMessage String
  | DragStart Position
  | DragAt Position
  | DragEnd Position
