module Menu.Messages exposing (Msg(..))

import Lobby.Types as Lobby


type Msg
    = Start Lobby.GameType
