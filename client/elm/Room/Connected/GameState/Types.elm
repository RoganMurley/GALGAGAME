module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (Model, Res, WhichPlayer)
import Model.ViewModel exposing (ViewModel)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | PlayingGame ( Model, ViewModel ) ( List Res, Float )
    | Ended (Maybe WhichPlayer) Model ViewModel (Maybe Model) ( List Res, Float )


type WaitType
    = WaitQuickplay
    | WaitCustom
