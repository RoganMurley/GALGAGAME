module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (Model, WhichPlayer)
import Resolvable.Types as Resolvable


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | Started PlayState


type PlayState
    = Playing Resolvable.Model
    | Ended Winner Resolvable.Model


type alias Winner =
    Maybe WhichPlayer


type WaitType
    = WaitQuickplay
    | WaitCustom
