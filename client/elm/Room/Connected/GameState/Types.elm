module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Resolvable.Types as Resolvable
import WhichPlayer.Types exposing (WhichPlayer)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | Started PlayState


type PlayState
    = Playing Resolvable.Model
    | Ended Winner Resolvable.Model (Maybe ReplayId)


type alias Winner =
    Maybe WhichPlayer


type alias ReplayId =
    String


type WaitType
    = WaitQuickplay
    | WaitCustom
