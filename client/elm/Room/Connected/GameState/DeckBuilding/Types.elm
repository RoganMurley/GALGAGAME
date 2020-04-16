module DeckBuilding.Types exposing (Character, Model, Rune, RuneCards)

import Card.Types exposing (Card)


type alias Model =
    { characters : List Character
    , index : Int
    , ready : Bool
    }


type alias Character =
    { name : String
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
