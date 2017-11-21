module Connected.Messages exposing (Msg(..))

import GameState.Messages as GameState
import Settings.Messages as Settings


type Msg
    = Concede
    | GameStateMsg GameState.Msg
    | SetVolume Int
    | SettingsMsg Settings.Msg
