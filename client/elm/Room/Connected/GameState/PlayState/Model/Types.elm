module Model.Types exposing (Life, Model)

import Hand.Types exposing (Hand)
import Stack.Types exposing (Stack)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { hand : Hand
    , otherHand : Int
    , deck : Int
    , otherDeck : Int
    , stack : Stack
    , turn : WhichPlayer
    , life : Life
    , otherLife : Life
    , maxLife : Life
    , otherMaxLife : Life
    , rot : Int
    }


type alias Life =
    Int
