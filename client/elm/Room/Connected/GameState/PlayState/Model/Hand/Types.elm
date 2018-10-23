module Hand.Types exposing (Hand, HandIndex)

import Card.Types exposing (Card)


type alias Hand =
    List Card


type alias HandIndex =
    { cardCount : Int
    , hover : Maybe Int
    , index : Int
    }
