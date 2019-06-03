module PlayState.Types exposing (PlayState(..))

import Game.Types as Game
import WhichPlayer.Types exposing (WhichPlayer)


type PlayState
    = Playing { game : Game.Model }
    | Ended
        { game : Game.Model
        , winner : Maybe WhichPlayer
        , replayId : Maybe String
        , xp : Maybe Int
        }
