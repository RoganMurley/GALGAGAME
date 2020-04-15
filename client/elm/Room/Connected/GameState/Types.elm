module GameState.Types exposing (GameState(..), WaitType(..))

import DeckBuilding.Types as DeckBuilding
import PlayState.Types exposing (PlayState)


type GameState
    = Waiting WaitType
    | Selecting DeckBuilding.Model
    | Started PlayState


type WaitType
    = WaitQuickplay
    | WaitCustom
