module Hand.Types exposing (..)

import Card.Types exposing (Card)


type alias Hand =
    List Card


type alias HoverCardIndex =
    Maybe Int
