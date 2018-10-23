module Stack.Types exposing (Stack, StackCard)

import Card.Types exposing (Card)
import WhichPlayer.Types exposing (WhichPlayer)


type alias StackCard =
    { owner : WhichPlayer
    , card : Card
    }


type alias Stack =
    List StackCard
