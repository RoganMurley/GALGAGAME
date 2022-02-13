module PlayState.Types exposing (PlayState(..), ResolveOutcomeInput)

import Aftermath.Types as Aftermath
import Buttons.Types exposing (Buttons)
import Game.Types as Game
import Model.Types exposing (Model)
import Resolvable.Types as Resolvable
import WhichPlayer.Types exposing (WhichPlayer)


type PlayState
    = Playing { game : Game.Model }
    | Ended
        { game : Game.Model
        , winner : Maybe WhichPlayer
        , replayId : Maybe String
        , aftermath : Aftermath.Model
        , buttons : Buttons
        }


type alias ResolveOutcomeInput =
    { resDiffList : List Resolvable.ResolveDiffData
    , initial : Model
    , finalState : PlayState
    }
