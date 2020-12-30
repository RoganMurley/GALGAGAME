module GameState.Messages exposing (Msg(..))

import DeckBuilding.Messages as DeckBuilding
import PlayState.Messages as PlayState


type Msg
    = PlayStateMsg PlayState.Msg
    | ResolveOutcome String
    | SelectingMsg DeckBuilding.Msg
    | Sync String
