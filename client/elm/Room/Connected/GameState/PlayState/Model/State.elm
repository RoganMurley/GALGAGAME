module Model.State exposing (init)

import Model.Types exposing (Model)
import Stack.State as Stack
import WhichPlayer.Types exposing (WhichPlayer(..))


init : Model
init =
    let
        defaultMaxLife =
            50
    in
    { hand = []
    , otherHand = 0
    , stack = Stack.init
    , turn = PlayerA
    , life = defaultMaxLife
    , otherLife = defaultMaxLife
    , maxLife = defaultMaxLife
    , otherMaxLife = defaultMaxLife
    , rot = 0
    }
