module PlayState.Types exposing (PlayState(..), ResolveOutcomeInput)

import Game.Types as Game
import Model.Types exposing (Model)
import Resolvable.Types as Resolvable
import Stats exposing (StatChange)
import WhichPlayer.Types exposing (WhichPlayer)


type PlayState
    = Playing { game : Game.Model }
    | Ended
        { game : Game.Model
        , winner : Maybe WhichPlayer
        , replayId : Maybe String
        , xp : Maybe StatChange
        }


type alias ResolveOutcomeInput =
    { resDiffList : List Resolvable.ResolveDiffData
    , initial : Model
    , finalState : PlayState
    }
