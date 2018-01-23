module Model.Types exposing (..)

import Card.Types exposing (Card)
import Hand.Types exposing (Hand)


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


type alias Stack =
    List StackCard


type WhichPlayer
    = PlayerA
    | PlayerB


type alias StackCard =
    { owner : WhichPlayer
    , card : Card
    }
