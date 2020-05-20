module DeckBuilding.Types exposing (Character, Characters, Model, Rune, RuneCards, RuneCursor(..), RuneSelectModel)

import Card.Types exposing (Card)


type alias Model =
    { characters : Characters
    , runes : List Rune
    , runeSelect : Maybe RuneSelectModel
    , ready : Bool
    }


type alias RuneSelectModel =
    { cursor : RuneCursor
    , selected : Rune
    }


type RuneCursor
    = RuneCursorA
    | RuneCursorB
    | RuneCursorC


type alias Characters =
    { previous : List Character
    , selected : Character
    , remaining : List Character
    }


type alias Character =
    { name : String
    , imgUrl : String
    , runeA : Rune
    , runeB : Rune
    , runeC : Rune
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
