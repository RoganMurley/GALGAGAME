module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (Model, Res, WhichPlayer)
import ViewModel.Types exposing (ViewModel)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | PlayingGame ( Model, ViewModel ) ( Res, Int )
    | Ended (Maybe WhichPlayer) Model ViewModel (Maybe Model) ( Res, Int )


type WaitType
    = WaitQuickplay
    | WaitCustom
