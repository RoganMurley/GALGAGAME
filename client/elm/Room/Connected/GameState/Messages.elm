module GameState.Messages exposing (Msg(..))

import DeckBuilding.Messages as DeckBuilding
import Mouse exposing (Position)
import PlayState.Messages as PlayState


type Msg
    = Mouse Position
    | MouseClick Position
    | PlayStateMsg PlayState.Msg
    | ResolveOutcome String
    | SelectingMsg DeckBuilding.Msg
    | Sync String
    | Touch (Maybe Position)
