module Model.State exposing (init)

import Model.Types exposing (Model)
import WhichPlayer.Types exposing (WhichPlayer(..))


init : Model
init =
    { hand = []
    , otherHand = 0
    , stack = []
    , turn = PlayerA
    , life = 100
    , otherLife = 100
    }
