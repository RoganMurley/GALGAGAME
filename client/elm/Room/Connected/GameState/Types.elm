module GameState.Types exposing (..)

import CharacterSelect.Types as CharacterSelect
import Game.Types as Game
import WhichPlayer.Types exposing (WhichPlayer)


type GameState
    = Waiting WaitType
    | Selecting CharacterSelect.Model
    | Started PlayState


type PlayState
    = Playing Game.Model
    | Ended Winner Game.Model (Maybe ReplayId)


type alias Winner =
    Maybe WhichPlayer


type alias ReplayId =
    String


type WaitType
    = WaitQuickplay
    | WaitCustom
