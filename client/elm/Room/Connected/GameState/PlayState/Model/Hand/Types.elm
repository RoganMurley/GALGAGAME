module Hand.Types exposing (Hand, OtherHand)

import Card.Types exposing (Card, KnowableCard)


type alias Hand =
    List KnowableCard


type alias OtherHand =
    List (Maybe Card)
