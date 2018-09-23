module GameState.Types exposing (..)

import Clock.Types as Clock
import CharacterSelect.Types as CharacterSelect
import WhichPlayer.Types exposing (WhichPlayer)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | Started PlayState


type PlayState
    = Playing Clock.Model
    | Ended Winner Clock.Model (Maybe ReplayId)


type alias Winner =
    Maybe WhichPlayer


type alias ReplayId =
    String


type WaitType
    = WaitQuickplay
    | WaitCustom
