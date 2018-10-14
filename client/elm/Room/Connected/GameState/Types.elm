module GameState.Types exposing (GameState(..), WaitType(..))

import CharacterSelect.Types as CharacterSelect
import PlayState.Types exposing (PlayState)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | Started PlayState


type WaitType
    = WaitQuickplay
    | WaitCustom
