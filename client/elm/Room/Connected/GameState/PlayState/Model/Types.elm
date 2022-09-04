module Model.Types exposing (Life, Model, Pass(..))

import Hand.Types exposing (Hand, OtherHand)
import Stack.Types exposing (Stack)
import WhichPlayer.Types exposing (WhichPlayer)


type alias Model =
    { hand : Hand
    , otherHand : OtherHand
    , deck : Int
    , otherDeck : Int
    , stack : Stack
    , turn : WhichPlayer
    , life : Life
    , otherLife : Life
    , maxLife : Life
    , otherMaxLife : Life
    , rot : Int
    , passes : Pass
    }


type Pass
    = NoPass
    | OnePass


type alias Life =
    Int
