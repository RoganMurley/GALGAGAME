module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Model.Types exposing (Model)
import Resolvable.Types as Resolvable
import WhichPlayer.Types exposing (WhichPlayer)


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
