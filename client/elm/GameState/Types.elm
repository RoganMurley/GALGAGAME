module GameState.Types exposing (GameState(..))

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (Model, Res, WhichPlayer)


type GameState
    = Waiting
    | Selecting CharacterSelect.Model
    | PlayingGame Model ( Res, Int )
    | Ended (Maybe WhichPlayer) (Maybe Model) ( Res, Int )
