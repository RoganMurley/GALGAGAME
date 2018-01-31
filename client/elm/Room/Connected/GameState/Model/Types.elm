module Model.Types exposing (..)

import Hand.Types exposing (Hand)
import Stack.Types exposing (Stack)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { hand : Hand
    , otherHand : Int
    , stack : Stack
    , turn : WhichPlayer
    , life : Life
    , otherLife : Life
    , otherHover : Maybe Int
    }


type alias Life =
    Int
