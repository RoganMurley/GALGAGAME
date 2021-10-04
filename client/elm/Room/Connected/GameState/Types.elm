module GameState.Types exposing (GameState(..))

import DeckBuilding.Types as DeckBuilding
import PlayState.Types exposing (PlayState)
import Waiting.Types as WaitType


type GameState
    = Waiting WaitType.Model
    | Selecting DeckBuilding.Model
    | Started PlayState
