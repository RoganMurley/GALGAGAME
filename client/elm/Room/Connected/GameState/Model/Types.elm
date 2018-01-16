module Model.Types exposing (..)

import Animation.Types exposing (Anim)
import Card.Types exposing (Card)


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


type alias Hand =
    List Card


type alias HoverCardIndex =
    Maybe Int


type alias StackCard =
    { owner : WhichPlayer
    , card : Card
    }
