module Model.State exposing (..)

import Model.Types exposing (FullModel, WhichPlayer(..))


init : FullModel
init =
    { hand = []
    , otherHand = 0
    , stack = []
    , turn = PlayerA
    , life = 100
    , otherLife = 100
    , otherHover = Nothing
    , diffLife = 0
    , diffOtherLife = 0
    }


maxHandLength : Int
maxHandLength =
    6
