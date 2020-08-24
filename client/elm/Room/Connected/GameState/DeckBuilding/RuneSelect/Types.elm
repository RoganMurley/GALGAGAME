module RuneSelect.Types exposing (Model, Rune, RuneCards, RuneCursor(..))

import Buttons.Types exposing (Buttons)
import Card.Types as Card exposing (Card)
import Carousel exposing (Carousel)


type alias Model =
    { cursor : RuneCursor
    , carousel : Carousel Rune
    , entities : List (Card.Entity {})
    , hover : Maybe (Card.Entity {})
    , buttons : Buttons
    }


type alias Rune =
    { name : String
    , desc : String
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
