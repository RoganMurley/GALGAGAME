module RuneSelect.Types exposing (Model, Rune, RuneCards, RuneCursor(..))

import Card.Types exposing (Card)
import Carousel exposing (Carousel)


type alias Model =
    { cursor : RuneCursor
    , carousel : Carousel Rune
    }


type alias Rune =
    { name : String
    , imgURL : String
    , cards : RuneCards
    }


type alias RuneCards =
    { a : Card
    , b : Card
    , c : Card
    , d : Card
    }


type RuneCursor
    = RuneCursorA
    | RuneCursorB
    | RuneCursorC
