module Hand.Types exposing (Hand, OtherHand)

import Card.Types exposing (Card)


type alias Hand =
    List Card


type alias OtherHand =
    List (Maybe Card)
