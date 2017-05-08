module Model.Types exposing (..)

import Card exposing (Card)


type alias Model =
    { hand : Hand
    , otherHand : Int
    , stack : Stack
    , turn : Turn
    , life : Life
    , otherLife : Life
    , otherHover : Maybe Int
    }


type alias ModelDiff a =
    { a
        | diffOtherLife : Life
        , diffLife : Life
    }


type alias FullModel =
    ModelDiff Model


type alias Life =
    Int


type alias Stack =
    List StackCard


type WhichPlayer
    = PlayerA
    | PlayerB


type alias Turn =
    WhichPlayer


type alias Hand =
    List Card


type alias HoverCardIndex =
    Maybe Int


type alias StackCard =
    { owner : WhichPlayer
    , card : Card
    }


type alias Res =
    List Model


type alias Intensity =
    { lower : Float
    , upper : Float
    }


intensity : FullModel -> Intensity
intensity m =
    { lower = (toFloat m.diffOtherLife) / 10
    , upper = (toFloat m.diffLife) / 10
    }
